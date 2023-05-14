//
// Created by Fan Long on 2020/12/6.
//

#ifndef MINICC_IRGENERATOR_H
#define MINICC_IRGENERATOR_H

//add more header files if your want
#include "ASTVisitor.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "Program.h"
#include <iostream>

namespace minicc {

    class IRGenerator : public ASTVisitor {
        std::unique_ptr<llvm::LLVMContext> TheContext;
        std::unique_ptr<llvm::Module> TheModule;
        std::unique_ptr<llvm::IRBuilder<>> TheBuilder;
        std::string ModuleName;

        //add your variables and member functions
        std::map<Expr* , llvm::Value*> exprTable;
        std::map<Statement* , llvm::BasicBlock*> blockTable;
    public:
        //modify if needed
        explicit IRGenerator(const std::string moduleName) : ASTVisitor(), TheModule(), TheBuilder(), ModuleName(moduleName){
            TheContext = std::make_unique<llvm::LLVMContext>();
        }

        std::unique_ptr<llvm::Module> getModule() {
            TheBuilder.reset();
            return std::move(TheModule);
        }

        llvm::Value* getValue(Expr* name) {
            auto it = exprTable.find(name);
            assert( it != exprTable.end());
            return it->second;
        }
        void addExprValue(Expr* name, llvm::Value* value) {
            exprTable.insert(std::make_pair(name, value));
        }

        llvm::BasicBlock* getBlock(Statement* name) {
            auto it = blockTable.find(name);
            assert( it != blockTable.end());
            return it->second;
        }
        void addBlock(Statement* name, llvm::BasicBlock* block) {
            blockTable.insert(std::make_pair(name, block));
        }

        llvm::Type* minicTollvmType(minicc::Type minicType)
        {
            if (minicType.toString() == "void") {
                return llvm::Type::getVoidTy(*TheContext);
            } else if (minicType.toString() == "bool") {
                return llvm::Type::getInt1Ty(*TheContext);
            } else if (minicType.toString() == "int") {
                return llvm::Type::getInt32Ty(*TheContext);
            } 

            std::cout << minicType.toString() << "  error ! \n\n";
            return llvm::Type::getVoidTy(*TheContext);
        } 

        llvm::Value* opcodeTollvmValue(std::string opcode, llvm::Value* a, llvm::Value* b){
            if (opcode=="+") { 
                return TheBuilder->CreateAdd(a, b);
            }
            if (opcode=="-") { 
                return TheBuilder->CreateSub(a, b);
            }
            if (opcode=="*"){
                return TheBuilder->CreateMul(a, b);
            }
            if (opcode=="/")
                {return TheBuilder->CreateSDiv(a, b);}
                if (opcode=="==")
                    {return TheBuilder->CreateICmpEQ(a, b);}
                if (opcode=="!=")
                    {return TheBuilder->CreateICmpNE(a, b);}
                if (opcode=="<")
                    {return TheBuilder->CreateICmpSLT(a, b);}
                if (opcode=="<=")
                    {return TheBuilder->CreateICmpSLE(a, b);}
                if (opcode==">")
                    {return TheBuilder->CreateICmpSGT(a, b);}
                if (opcode==">=")
                    {return TheBuilder->CreateICmpSGE(a, b);}
                return TheBuilder->CreateAdd(a, b);
            
        }

        void visitProgram(Program* prog) override;

        void visitVarDecl(VarDeclaration *decl) override;

        void visitFuncDecl(FuncDeclaration *func) override;

        void visitIfStmt(IfStatement *stmt) override;

        void visitForStmt(ForStatement *stmt) override;

        void visitWhileStmt(WhileStatement *stmt) override;

        void visitReturnStmt(ReturnStatement *stmt) override;

        void visitBreakStmt(BreakStatement *stmt) override;

        void visitScope(ScopeStatement *stmt) override;

        void visitUnaryExpr(UnaryExpr *expr) override;

        void visitBinaryExpr(BinaryExpr *expr) override;

        void visitCallExpr(CallExpr *expr) override;

        void visitVarExpr(VarExpr *expr) override;

        void visitAssignmentExpr(AssignmentExpr *expr) override;

        void visitIntLiteralExpr(IntLiteralExpr *literal) override;

        void visitBoolLiteralExpr(BoolLiteralExpr *literal) override;
    };
}

#endif //MINICC_IRGENERATOR_H
