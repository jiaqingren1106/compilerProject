//
// Created by Fan Long on 2020/12/6.
//

//add more header files if your want
#include "IRGenerator.h"
#include "llvm/IR/Module.h"
#include "Declarations.h"
#include "Program.h"
#include "Exprs.h"
#include "Statements.h"
#include <iostream>


namespace minicc {
    //add your member helper functions

    void IRGenerator::visitProgram(Program *prog) {
        //std::cout<<"visitProgram \n";
        //Initlize llvm module and builder
        TheModule = std::make_unique<llvm::Module>(ModuleName, *TheContext);
        TheBuilder = std::make_unique<llvm::IRBuilder<>>(*TheContext);

        //start your code here
        //Insert all of functions into TheModule
        FuncSymbolTable* function_table = prog->funcTable();

        FuncSymbolTable::Iterator it;
        for (it = function_table->begin(); it != function_table->end(); it++){
            // convert minic type to llvm type
            llvm::Type* llvmType =  minicTollvmType(it->second.ReturnType);

            std::vector<llvm::Type*> func_param;
            std::vector<Type> param_vector = it->second.ParameterTypes;
            for (int i=0; i< param_vector.size();i++) {
                func_param.push_back(minicTollvmType(param_vector[i]));
            }

            llvm::FunctionType* func_type = llvm::FunctionType::get(llvmType, func_param, false);
            llvm::Function* func = llvm::Function::Create(func_type,llvm::Function::ExternalLinkage, it->first, TheModule.get());
        }
        ASTVisitor::visitProgram(prog);
    }

    void IRGenerator::visitVarDecl(VarDeclaration *decl) {
        //std::cout<<"visitVarDecl \n";
        //start your code here
        ASTVisitor::visitVarDecl(decl);

        VarSymbolTable* vartable = decl->getParent()->scopeVarTable();
        llvm::Type* llvmType =  minicTollvmType(dynamic_cast<TypeReference*>(decl->getChild(0))->value());

        //Check the variables are global or local
        bool global = decl->getParent()->isProgram();

        //Check it is array or not
        //Create llvm::Value and set them into variable symbol table
        for (int i=0; i < decl->numVarReferences();i++) {
            VarReference * var = decl->varReference(i);
            bool isarray = var->isArray();
            std::string var_name = var->identifier()->name();

            if (vartable->containsVar(var->identifier()->name())) {
                llvm::Constant* const_value =  llvm::ConstantInt::get(llvmType, 0, true);
                llvm::ConstantAggregateZero* const_value_vec;

                // why constvalue for global and alloca value for loccal
                if (global) { 
                    if (isarray) {
                        llvm::ArrayType* arrayType = llvm::ArrayType::get(llvmType, dynamic_cast<IntLiteralExpr*>(var->indexExpr())->value());
                        const_value_vec = llvm::ConstantAggregateZero::get(arrayType);
                        auto* glboal = new llvm::GlobalVariable(*TheModule, arrayType, false, llvm::GlobalVariable::CommonLinkage, const_value_vec, var_name);
                        vartable->setLLVMValue(var_name, glboal);
                    } else {
                        auto* glboal = new llvm::GlobalVariable(*TheModule, llvmType, false, llvm::GlobalVariable::CommonLinkage, const_value, var_name);
                        vartable->setLLVMValue(var_name, glboal);
                    }
                } else {
                    llvm::Value* value;
                    if (isarray){
                        llvm::ArrayType* arrayType = llvm::ArrayType::get(llvmType, dynamic_cast<IntLiteralExpr*>(var->indexExpr())->value());
                        const_value_vec = llvm::ConstantAggregateZero::get(arrayType);
                        value = TheBuilder->CreateAlloca(arrayType, nullptr, var_name);
                    }else {
                        value = TheBuilder->CreateAlloca(llvmType, nullptr, var_name);
                    }
                    vartable->setLLVMValue(var_name, value);
                }
            }
        }
    }

    void IRGenerator::visitFuncDecl(FuncDeclaration *func) {
        //std::cout<<"visitFuncDecl \n";
        //start your code here
        std::string func_name = func->name();
        bool hasBody = func->hasBody();
        Type return_type =  func->returnType();
        size_t param_size = func->numParameters();
        size_t numChildren = func->numChildren();

        //Get the corresponding llvm::Function object
        llvm::Function* llvm_function = TheModule->getFunction(func_name);
       
        //If having body, a entry basic block should be created for the 
        //function and inserted in TheBuilder.
        if (hasBody) {
            ASTNode* scope = func->getChild(numChildren-1);
            size_t scope_children = scope->numChildren();
            VarSymbolTable* vartable = scope->scopeVarTable();

            llvm::BasicBlock* bb = llvm::BasicBlock::Create(*TheContext, "entry", llvm_function);
            TheBuilder->SetInsertPoint(bb);

            //Check the function declaration has body or not. If so, allocate 
            //parameter variables and set LLVM in symbol table.
            std::vector<llvm::Value*> value_vec;
            for (int i=0; i<param_size; i++) {                
                Parameter* func_param = func->parameter(i);
                std::string paramName = func_param->identifier()->name();
                llvm::Type* llvmType =  minicTollvmType(func_param->type());
                llvm::Value* value = TheBuilder->CreateAlloca(llvmType, nullptr, func_param->name());
                value_vec.push_back(value);
                vartable->setLLVMValue(paramName, value);
            }
            
            int i=0;
            for (auto it = llvm_function->arg_begin(); it !=llvm_function->arg_end(); it++) { 
                TheBuilder->CreateStore(it, value_vec[i]);
                i++;
            }
            ASTVisitor::visitFuncDecl(func);

            //f having body but no return expr in void function, create a void return for it.
            if (return_type.isVoid() && ( scope_children==0 || !scope->getChild(scope_children-1)->isReturn())) {
                TheBuilder->CreateRetVoid();
            }

        }
    }

    void IRGenerator::visitIfStmt(IfStatement *stmt) {
        //start your code here
        //std::cout<<"visitIfStmt \n";

        bool has_else = stmt->hasElse();
        Expr* condExpr = stmt->condExpr();
        Statement* thenStmt = stmt->thenStmt();

        FuncDeclaration* parentFunc = stmt->getParentFunction();
        llvm::Function* llvm_function = TheModule->getFunction(parentFunc->name());
        llvm::BasicBlock* bb = TheBuilder->GetInsertBlock();

        //If having "else" statement, three basic blocks are created to represent
        //"then" block, "else" block, "after" block.
        //CreateCondBr is needed.
        if (has_else) {
            Statement* elseStmt = stmt->elseStmt();
            llvm::BasicBlock *then_bb = llvm::BasicBlock::Create(*TheContext, "then", llvm_function);
            llvm::BasicBlock *else_bb = llvm::BasicBlock::Create(*TheContext, "else",llvm_function);
            llvm::BasicBlock *out_bb = llvm::BasicBlock::Create(*TheContext, "after",llvm_function);

            condExpr->accept(this);
            llvm::Value* cond_value = getValue(condExpr);
            TheBuilder->CreateCondBr(cond_value, then_bb, else_bb);

            TheBuilder->SetInsertPoint(then_bb);
            thenStmt->accept(this);
            if ( !TheBuilder->GetInsertBlock()->getTerminator() ) {
                TheBuilder->CreateBr(out_bb);
            }

            TheBuilder->SetInsertPoint(else_bb);
            elseStmt->accept(this);
            if ( !TheBuilder->GetInsertBlock()->getTerminator()) {
                TheBuilder->CreateBr(out_bb);
            }    

            TheBuilder->SetInsertPoint(out_bb);

        } else {
            // not done;
            llvm::BasicBlock *then_bb = llvm::BasicBlock::Create(*TheContext, "then", llvm_function);
            llvm::BasicBlock *out_bb = llvm::BasicBlock::Create(*TheContext, "after", llvm_function);

            condExpr->accept(this);
            llvm::Value* cond_value = getValue(condExpr);
      
            TheBuilder->CreateCondBr(cond_value, then_bb, out_bb);

            TheBuilder->SetInsertPoint(then_bb);
            thenStmt->accept(this);

            if (!TheBuilder->GetInsertBlock()->getTerminator()) {
                TheBuilder->CreateBr(out_bb);
            } 
            TheBuilder->SetInsertPoint(out_bb);
        }
    }

    void IRGenerator::visitForStmt(ForStatement *stmt) {
        //std::cout<<"visitForStmt \n";
        //start your code here
        Expr* initExpr = stmt->initExpr();
        Expr* condExpr = stmt->condExpr();
        Expr* iterExpr = stmt->iterExpr();
        Statement* child_body = stmt->body();

        FuncDeclaration* parentFunc = stmt->getParentFunction();
        llvm::Function* llvm_function = TheModule->getFunction(parentFunc->name());

        llvm::BasicBlock* bb = TheBuilder->GetInsertBlock();
        llvm::BasicBlock* cond_bb =  llvm::BasicBlock::Create(*TheContext, "cond", llvm_function);
        llvm::BasicBlock* body_bb =  llvm::BasicBlock::Create(*TheContext, "body", llvm_function);
        llvm::BasicBlock* out_bb =  llvm::BasicBlock::Create(*TheContext, "out", llvm_function);  

        if (initExpr) initExpr->accept(this);
        TheBuilder->CreateBr(cond_bb);

        TheBuilder->SetInsertPoint(cond_bb);

        if (condExpr) {condExpr->accept(this);
        llvm::Value* cond_value = getValue(condExpr);
        TheBuilder->CreateCondBr(cond_value, body_bb, out_bb);}
        else {
            TheBuilder->CreateBr(body_bb);
        }

        TheBuilder->SetInsertPoint(body_bb);
        addBlock(stmt, out_bb);
        child_body->accept(this);
        if (!TheBuilder->GetInsertBlock()->getTerminator()) {
            if (iterExpr) iterExpr->accept(this);
            TheBuilder->CreateBr(cond_bb);
        }
        
        TheBuilder->SetInsertPoint(out_bb);
    }

    void IRGenerator::visitWhileStmt(WhileStatement *stmt) {
        //std::cout<<"visitWhileStmt \n";
        //start your code here
        Expr* child_expr = stmt->condExpr();
        Statement* child_body = stmt->body();
        FuncDeclaration* parentFunc = stmt->getParentFunction();
        llvm::Function* llvm_function = TheModule->getFunction(parentFunc->name());

        llvm::BasicBlock* bb = TheBuilder->GetInsertBlock();
        llvm::BasicBlock* cond_bb = llvm::BasicBlock::Create(*TheContext, "cond", llvm_function);
        llvm::BasicBlock* body_bb = llvm::BasicBlock::Create(*TheContext, "body", llvm_function);
        llvm::BasicBlock* exit_bb = llvm::BasicBlock::Create(*TheContext, "exit", llvm_function);  

        TheBuilder->CreateBr(cond_bb);
        
        TheBuilder->SetInsertPoint(cond_bb);
        if (child_expr) {child_expr->accept(this);
        llvm::Value* cond_value = getValue(child_expr);
        TheBuilder->CreateCondBr(cond_value, body_bb, exit_bb);}
        else {
            TheBuilder->CreateBr(body_bb);
        }

        TheBuilder->SetInsertPoint(body_bb);
        addBlock(stmt, exit_bb);
        child_body->accept(this);
        if (!TheBuilder->GetInsertBlock()->getTerminator()) {
            TheBuilder->CreateBr(cond_bb);
        }

        TheBuilder->SetInsertPoint(exit_bb);
    }

    void IRGenerator::visitReturnStmt(ReturnStatement *stmt) {
        //std::cout<<"visitReturnStmt \n";

        //start your code here
        ASTVisitor::visitReturnStmt(stmt);

        //CreateRet or CreateRetVoid
        if (stmt->hasReturnExpr()) {
            llvm::Value* value = getValue(stmt->returnExpr());
            TheBuilder->CreateRet(value);
        } else {
            llvm::Value* voidReturn = TheBuilder->CreateRetVoid();
        }
    }

    void IRGenerator::visitBreakStmt(BreakStatement *stmt) {
        //std::cout<<"visitBreakStmt \n";

        //start your code here
        ASTVisitor::visitBreakStmt(stmt);

        ForStatement* for_parent = stmt->getParentForStatement();
        WhileStatement* while_parent = stmt->getParentWhileStatement();

        if (!for_parent && !while_parent){
            //std::cout << "break stmt does not belong to any loop scope!!\n";
        }else if (for_parent) {
            TheBuilder->CreateBr(getBlock(for_parent));
        }else if (while_parent) {
            TheBuilder->CreateBr(getBlock(while_parent));
        }else {
            if (for_parent->getParentWhileStatement() == while_parent) {
                TheBuilder->CreateBr(getBlock(for_parent));
            }else {
                TheBuilder->CreateBr(getBlock(while_parent));
            }
        }
    }


    void IRGenerator::visitUnaryExpr(UnaryExpr *expr) {
        //std::cout<<"visitUnaryExpr \n";
        //start your code here
        ASTVisitor::visitUnaryExpr(expr);

        //CreateNeg or CreateNot
        std::string op = Expr::opcodeToString(expr->opcode());
        Expr* child_expr = dynamic_cast<Expr*> (expr->getChild(0));
        llvm::Value* res;
        if (op == "-" ) {
            res = TheBuilder->CreateNeg(getValue(child_expr));
        } else if (op == "!") {
            res = TheBuilder->CreateNot(getValue(child_expr));
        } else {
            //std::cout << "Unary error!\n";
        }
        addExprValue(expr, res);
    }

    void IRGenerator::visitBinaryExpr(BinaryExpr *expr) {
        //std::cout<<"visitBinaryExpr \n";

        std::string op = Expr::opcodeToString(expr->opcode());
        Expr* left_expr = dynamic_cast<Expr*> (expr->getChild(0));
        Expr* right_expr = dynamic_cast<Expr*> (expr->getChild(1));
        FuncDeclaration* parentFunc = expr->getParentFunction();
        llvm::Function* llvm_function = TheModule->getFunction(parentFunc->name());

        //start your code here
        // ASTVisitor::visitBinaryExpr(expr);
        left_expr->accept(this);
        llvm::Value* leftValue = getValue(left_expr);
        llvm::Value* rightValue;

        //If binary op is not "AND" or "OR", create the corresponding instructions.
        if (op != "&&" && op != "||") {
            right_expr->accept(this);
            rightValue= getValue(right_expr);
            llvm::Value* res = opcodeTollvmValue(op, leftValue,rightValue);
            addExprValue(expr, res);
            return;
        }

        llvm::BasicBlock* bb = TheBuilder->GetInsertBlock();
        // llvm::BasicBlock* currentBlock = llvm::BasicBlock::Create(*TheContext, "current", llvm_function);
        llvm::BasicBlock* slow_bb    = llvm::BasicBlock::Create(*TheContext, "slow", llvm_function);
        llvm::BasicBlock* out_bb     = llvm::BasicBlock::Create(*TheContext, "out", llvm_function);
        llvm::PHINode* node;  

        TheBuilder->SetInsertPoint(bb);

        if (op == "&&") {
            TheBuilder->CreateCondBr(leftValue, slow_bb, out_bb);
            
            TheBuilder->SetInsertPoint(slow_bb);
            right_expr->accept(this);
            rightValue = getValue(right_expr);

            TheBuilder->CreateBr(out_bb);

            TheBuilder->SetInsertPoint(out_bb);
            node = TheBuilder->CreatePHI(llvm::Type::getInt1Ty(*TheContext), 2);
            node->addIncoming(rightValue, slow_bb);
            node->addIncoming(llvm::ConstantInt::get(llvm::Type::getInt1Ty(*TheContext), 0,true), bb);
        } 
        else if (op == "||") {
            TheBuilder->CreateCondBr(leftValue, out_bb, slow_bb);

            TheBuilder->SetInsertPoint(slow_bb);
            right_expr->accept(this);
            rightValue= getValue(right_expr);
            TheBuilder->CreateBr(out_bb);

            TheBuilder->SetInsertPoint(out_bb);
            node = TheBuilder->CreatePHI(llvm::Type::getInt1Ty(*TheContext), 2);
            node->addIncoming(rightValue, slow_bb);
            node->addIncoming(llvm::ConstantInt::get(llvm::Type::getInt1Ty(*TheContext), 1,true), bb);
        }
        // TheBuilder->SetInsertPoint(bb);

        addExprValue(expr, node);
    }

    void IRGenerator::visitCallExpr(CallExpr *expr) {
        //std::cout<<"visitCallExpr \n";
        //start your code here
        ASTVisitor::visitCallExpr(expr);

        //Get Function from TheModule and createCall
        llvm::Function* llvm_func = TheModule->getFunction(expr->callee()->name());

        std::vector<llvm::Value*> arg_list;
        for (int i=0; i < expr->numArgs();i++) {
            llvm::Value* expr_val = getValue(expr->arg(i));
            arg_list.push_back(expr_val);
        }
        llvm::Value* val = TheBuilder->CreateCall(llvm_func, arg_list);
        addExprValue(expr, val);
    }

    void IRGenerator::visitVarExpr(VarExpr *expr) {
        //std::cout<<"visitVarExpr \n";

        //start your code here
        ASTVisitor::visitVarExpr(expr);

        VarReference* child_var = dynamic_cast<VarReference*> (expr->getChild(0));
        std::string var_name = child_var->identifier()->name();
        bool isArray = child_var->isArray();
        VarSymbolTable* var_table = expr->locateDeclaringTableForVar(var_name);

        //Acquire llvm:Value for variable.
        VarSymbolEntry var_entry = var_table->get(var_name);
        llvm::Value* var_value = var_entry.LLVMValue;
        llvm::Type* var_type;

        llvm::Value* res;
        if (isArray) {
            //CreateGEP for array.
            Expr* indexExpr = child_var->indexExpr();
            std::vector<llvm::Value*> idxlist;
            llvm::Value* fstidx = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0, true);
            llvm::Value* secidx = getValue(indexExpr);
            idxlist.push_back(fstidx);
            idxlist.push_back(secidx);

            var_type = minicTollvmType(var_entry.VarType.getIndexedType());
            size_t arrayBound = var_entry.VarType.arrayBound();
            llvm::ArrayType* arrayType = llvm::ArrayType::get(var_type, arrayBound);

            llvm::Value* idxptr = TheBuilder->CreateGEP(arrayType, var_value, idxlist);
            res = TheBuilder->CreateLoad(var_type, idxptr); 
            addExprValue(expr, res);
        } else {
            //CreateLoad
            var_type = minicTollvmType(var_entry.VarType);
            res = TheBuilder->CreateLoad(var_type, var_value); 
            addExprValue(expr, res);
        }
    }

    void IRGenerator::visitAssignmentExpr(AssignmentExpr *expr) {
        //std::cout<<"visitAssignmentExpr \n";
        //start your code here
        ASTVisitor::visitAssignmentExpr(expr);

        Expr* child_expr = dynamic_cast<Expr*> (expr->getChild(1));
        llvm::Value* llvm_expr_val = getValue(child_expr);

        VarReference* child_var = dynamic_cast<VarReference*> (expr->getChild(0));
        std::string var_name = child_var->identifier()->name();
        bool isArray = child_var->isArray();
        VarSymbolTable* var_table = expr->locateDeclaringTableForVar(var_name);

        //Get variable llvm::Value
        VarSymbolEntry var_entry = var_table->get(var_name);
        llvm::Value* llvm_var_val = var_entry.LLVMValue;
        llvm::Type* var_type;

        llvm::Value* res;
        if (isArray) {
            //CreateGEP for array.
            Expr* indexExpr = child_var->indexExpr();
            std::vector<llvm::Value*> idxlist;
            llvm::Value* fstidx = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0, true);
            llvm::Value* secidx = getValue(indexExpr);

            idxlist.push_back(fstidx);
            idxlist.push_back(secidx);

            var_type = minicTollvmType(var_entry.VarType.getIndexedType());
            size_t arrayBound = var_entry.VarType.arrayBound();
            llvm::ArrayType* arrayType = llvm::ArrayType::get(var_type, arrayBound);

            llvm::Value* idxptr = TheBuilder->CreateGEP(arrayType, llvm_var_val, idxlist);
            llvm::Value* res = TheBuilder->CreateStore(llvm_expr_val, idxptr);

            addExprValue(expr, res);
        } else {
            //CreateStore to assign the right value to the variable.
            var_type = minicTollvmType(var_entry.VarType);
            llvm::Value* res = TheBuilder->CreateStore(llvm_expr_val, llvm_var_val);
            addExprValue(expr, res);
        }
    }

    void IRGenerator::visitIntLiteralExpr(IntLiteralExpr *literal) {
        //std::cout<<"visitIntLiteralExpr \n";

        //start your code here
        ASTVisitor::visitIntLiteralExpr(literal);

        //Create a 32-bit Constant object.
        addExprValue(literal, llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), literal->value(), true));
    }

    void IRGenerator::visitBoolLiteralExpr(BoolLiteralExpr *literal) {
        //std::cout<<"visitBoolLiteralExpr \n";

        //start your code here
        ASTVisitor::visitBoolLiteralExpr(literal);

        //Create a 1-bit Constant object.
        addExprValue(literal, llvm::ConstantInt::get(llvm::Type::getInt1Ty(*TheContext), (int)(literal->value()), true));
    }

    void IRGenerator::visitScope(ScopeStatement *stmt) {
        //std::cout<<"visitScope \n";
        //start your code here
        size_t numChildren = stmt->numChildren();
        bool stop = false;
        for (size_t i=0; i < numChildren; i++) {
            ASTNode* node = stmt->getChild(i);
            if (!stop) {
                node->accept(this);
            }
            if (node->isReturn() || typeid(*node) == typeid(BreakStatement)) {
                stop = true;
            }
        }
    }

}