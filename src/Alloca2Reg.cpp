
#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/IR/CFG.h"
#include <set>
#include <map>
#include "llvm/IR/Constants.h"
#include <iostream>


using namespace llvm;

namespace {
    // A pass to replace alloca instructions with register-based instructions.
    struct Alloca2RegPass : public FunctionPass {
        static char ID;
        Alloca2RegPass() : FunctionPass(ID) {}

        // A set of alloca instructions that will be replaced.
        std::set<AllocaInst*> TargetAllocas;
        // A map to store the latest value for each alloca in a basic block.
        std::map<BasicBlock*, std::map<AllocaInst*, Value*> > Post;
        // A map to store the phi nodes for each alloca in a basic block.
        std::map<BasicBlock*, std::map<AllocaInst*, PHINode*> > Pre;

        // Collects all integer-type alloca instructions in a function.
        void collectTargetAllocas(Function &F) {
            Function::iterator bb_it;
            for (bb_it = F.begin(); bb_it != F.end(); bb_it++) {
                BasicBlock::iterator ins_it;
                for (ins_it = bb_it->begin(); ins_it != bb_it->end(); ins_it++) {
                    if (AllocaInst* alloca_ins = dyn_cast<AllocaInst>(ins_it)) {
                        if (alloca_ins->getAllocatedType()->isIntegerTy()) {
                            TargetAllocas.insert(alloca_ins);
                        }
                    }
                }
            }
        }

        // This function creates Phi Nodes for all Alloca instructions that are targets for replacement.
        void createPhiForAlloca(Function::iterator &bb_it, Function &F ,std::map<AllocaInst*, PHINode*> &Pre_data, std::map<AllocaInst*, Value*> &Post_data) {
            llvm::BasicBlock* bb = &(*bb_it);
            for (AllocaInst* alloca : TargetAllocas) {
                if (bb_it != F.begin()) {
                    llvm::PHINode* phi_node = PHINode::Create(alloca->getAllocatedType(), 0, alloca->getName().str(), &*(bb->begin()));

                    Pre_data.insert(std::make_pair(alloca, phi_node));
                    Post_data.insert(std::make_pair(alloca, phi_node));
                } else {
                    Pre_data.insert(std::make_pair(alloca, nullptr));
                    Post_data.insert(std::make_pair(alloca, llvm::ConstantInt::get(alloca->getAllocatedType(), 0)));
                }
            }
        }

        // This function adds values to Phi nodes. It iterates over each basic block in the function,
        // and for each Alloca instruction in a block, it iterates over the block's predecessors.
        // The last value assigned to the Alloca in each predecessor block is added to the Phi node as an incoming value.
        void addValueToPhi() {            
            for (auto pair : Pre) {
                BasicBlock* bb_i = pair.first;
                // Add incoming values to Phi Nodes.
                std::map<AllocaInst*, PHINode*> table = pair.second;

                // For each Alloca instruction in the basic block, iterate over its predecessors.
                for (auto &pair2 : table) {
                    AllocaInst* alloca = pair2.first;
                    PHINode* phi_node = pair2.second;

                    // Iterate over each predecessor basic block.
                    for (llvm::pred_iterator it = llvm::pred_begin(bb_i), end = llvm::pred_end(bb_i); it != end; ++it) {
                        llvm::BasicBlock* pred_bb = *it;
                        llvm::Value* incoming_value = Post[pred_bb][alloca]; 
                        phi_node->addIncoming(incoming_value, pred_bb);
                    }
                }
            }
        }

        // This function finds instructions that need to be removed from the function. 
        // These are Alloca, Store, and Load instructions related to the target Alloca instructions.
        void findRemovalIns(Function::iterator &bb_it, Function &F, std::vector <Instruction*> &trash_bin) {
            for (bb_it = F.begin(); bb_it != F.end(); bb_it++) {
                llvm::BasicBlock* bb = &(*bb_it);
                BasicBlock::iterator ins_it;
                for (ins_it = bb_it->begin(); ins_it != bb_it->end(); ins_it++) {
                    // Check if the instruction is an Alloca instruction.
                    if (AllocaInst* alloca_ins = dyn_cast<AllocaInst>(ins_it); TargetAllocas.count(alloca_ins) > 0) {
                        trash_bin.push_back(alloca_ins);
                    }
                    // Check if the instruction is a Store instruction.
                    if (StoreInst* store_ins = dyn_cast<StoreInst>(ins_it)) {
                        llvm::AllocaInst* store_name = dyn_cast<AllocaInst>(store_ins->getPointerOperand());
                        if (TargetAllocas.count(store_name) > 0) {
                            trash_bin.push_back(store_ins);
                        }
                    }
                    // Check if the instruction is a Load instruction.
                    if (LoadInst* load_ins = dyn_cast<LoadInst>(ins_it)) {
                        llvm::AllocaInst* load_name = dyn_cast<AllocaInst>(load_ins->getPointerOperand());
                        if (TargetAllocas.count(load_name) > 0) {
                            trash_bin.push_back(load_ins);
                        }
                    }
                }
            }
        }


        // Implements the pass for replacing alloca instructions with register-based instructions.
        virtual bool runOnFunction(Function &F) {

            // Clear all data structures before processing the function.
            Post.clear();
            Pre.clear();
            TargetAllocas.clear();

            // Collect all the target alloca instructions in the function.
            collectTargetAllocas(F);

            // Iterate over each basic block in the function.
            Function::iterator bb_it;
            for (bb_it = F.begin(); bb_it != F.end(); bb_it++) {
                BasicBlock::iterator ins_it;
                std::map<AllocaInst*, Value*> LastStored;
                std::map<AllocaInst*, PHINode*> Pre_data;
                std::map<AllocaInst*, Value*> Post_data;

                // Create Phi Nodes for target alloca instructions in each basic block.
                createPhiForAlloca(bb_it, F ,Pre_data,Post_data);

                // Iterate over instructions in the basic block.
                for (ins_it = bb_it->begin(); ins_it != bb_it->end(); ins_it++) {
                    // Process store instructions.
                    if (StoreInst* store_ins = dyn_cast<StoreInst>(ins_it)) {

                        llvm::AllocaInst* store_name = dyn_cast<AllocaInst>(store_ins->getPointerOperand());
                        if (TargetAllocas.count(store_name) == 0) {
                            continue;
                        }

                        // Update the last stored value for the target alloca instruction.
                        llvm::Value* value = store_ins->getValueOperand();
                        auto it = LastStored.find(store_name);
                        if (it!=LastStored.end()) {
                            it->second = value;
                        } else {
                            LastStored.insert(std::make_pair(store_name, value));
                        }
                        Post_data[store_name] = value;
                    }

                    // Process load instructions.
                    if (LoadInst* load_ins = dyn_cast<LoadInst>(ins_it)) {

                        llvm::AllocaInst* load_name = dyn_cast<AllocaInst>(load_ins->getPointerOperand());
                        if (TargetAllocas.count(load_name) == 0) {
                            continue;
                        }
                        // Replace load instructions with last stored value or Phi Node.
                        auto it = LastStored.find(load_name);
                        if (it!= LastStored.end()) {
                            load_ins->replaceAllUsesWith(it->second);
                        } else {
                            auto pair = Pre_data.find(load_name);
                            load_ins->replaceAllUsesWith(pair->second);
                        }
                    }
                }
                // Store the pre and post data for the current basic block.
                Pre.insert(std::make_pair(&(*bb_it), Pre_data));
                Post.insert(std::make_pair(&(*bb_it), Post_data));
            }

            // Add incoming values to Phi Nodes.
            addValueToPhi();

            // Prepare to remove all the Alloca, Store, and Load instructions related to the target Allocas.
            std::vector <Instruction*> trash_bin;
            findRemovalIns(bb_it, F, trash_bin);

            // Finally, remove all the unwanted instructions.
            for (auto trash : trash_bin) {
                trash->eraseFromParent();
            }

            return true;
        }
    };
}

char Alloca2RegPass::ID = 0;

static RegisterPass<Alloca2RegPass> X("alloca2reg", "Alloca-to-register pass for minic", false, false);

// Automatically enable the pass.
// http://adriansampson.net/blog/clangpass.html
static void registerAlloca2RegPass(const PassManagerBuilder &,
                                    legacy::PassManagerBase &PM) {
    PM.add(new Alloca2RegPass());
}
static RegisterStandardPasses
        RegisterMyPass(PassManagerBuilder::EP_EarlyAsPossible,
                       registerAlloca2RegPass);











