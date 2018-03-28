#**********************************************************
#**********************************************************
#**                                                      **
#**     ---- makefile ----                               **
#**                                                      **
#**     LEIBNIZ INSTITUTE (FBN)                          **
#**     Dummerstorf                                      **
#**     Start: 13 March 2018                             **
#**     Author: Jan Klosa                                **
#**                                                      **
#**********************************************************
#**********************************************************

OBJ     = MODULE_LVL1_KIND_NUMBERS.o\
          MODULE_LVL2_CREATE_FORMATTINGS.o\
          MODULE_LVL2_ON_SCREEN_MESSAGES.o\
          MODULE_LVL3_MEMORY_MANAGEMENT.o\
          MODULE_LVL3_READ_OPERATIONS.o\
          MODULE_LVL3_WRITE_OPERATIONS.o\
          MODULE_LVL4_MATRIX_OPERATIONS.o\
          MODULE_LVL5_FUNCTIONS.o\
          MODULE_TOP_LASSO.o\
          MODULE_TOP_GROUP_LASSO.o\
          MODULE_TOP_SPARSE_GROUP_LASSO.o\
          MODULE_TOP_MODIFIED_SG_LASSO.o\
          LASSO.o
MODS    = module_lvl1_kind_numbers.mod\
          module_lvl2_create_formattings.mod\
          module_lvl2_on_screen_messages.mod\
          module_lvl3_memory_management.mod\
          module_lvl3_read_operations.mod\
          module_lvl3_write_operations.mod\
          module_lvl4_matrix_operations.mod\
          module_lvl5_functions.mod\
          module_top_lasso.mod\
          module_top_group_lasso.mod\
          module_top_sparse_group_lasso.mod\
          module_top_modified_sg_lasso.mod
MOD_L1  = module_lvl1_kind_numbers.mod
MOD_L2  = module_lvl2_create_formattings.mod\
          module_lvl2_on_screen_messages.mod
MOD_L3  = module_lvl3_memory_management.mod\
          module_lvl3_read_operations.mod\
          module_lvl3_write_operations.mod
MOD_L4  = module_lvl4_matrix_operations.mod
MOD_L5  = module_lvl5_functions.mod
#FC      = gfortran-4.8
#LOADLIB = -llapack -lblas
FC      = ifort -CB
LOADLIB = /projekte/I2-NWG_GDA/klosa/lapack-3.6.1

#**********************************************************
#**     Create executable file:                          **
#**********************************************************
LASSO: $(OBJ)
#	$(FC) -o LASSO $(OBJ) $(LOADLIB)
	$(FC) -o LASSO $(OBJ) $(LOADLIB)/liblapack.a $(LOADLIB)/librefblas.a

#**********************************************************
#**     Edit dependencies of 1st degree:                 **
#**********************************************************
MODULE_LVL1_KIND_NUMBERS.mod: MODULE_LVL1_KIND_NUMBERS.o Modules/MODULE_LVL1_KIND_NUMBERS.f90
	$(FC) -c Modules/MODULE_LVL1_KIND_NUMBERS.f90
MODULE_LVL1_KIND_NUMBERS.o: Modules/MODULE_LVL1_KIND_NUMBERS.f90
	$(FC) -c Modules/MODULE_LVL1_KIND_NUMBERS.f90

#**********************************************************
#**     Edit dependencies of 2nd degree:                 **
#**********************************************************
MODULE_LVL2_CREATE_FORMATTINGS.mod: $(MOD_L1) MODULE_LVL2_CREATE_FORMATTINGS.o Modules/MODULE_LVL2_CREATE_FORMATTINGS.f90
	$(FC) -c Modules/MODULE_LVL2_CREATE_FORMATTINGS.f90
MODULE_LVL2_CREATE_FORMATTINGS.o: $(MOD_L1) Modules/MODULE_LVL2_CREATE_FORMATTINGS.f90
	$(FC) -c Modules/MODULE_LVL2_CREATE_FORMATTINGS.f90
MODULE_LVL2_ON_SCREEN_MESSAGES.mod: $(MOD_L1) MODULE_LVL2_ON_SCREEN_MESSAGES.o Modules/MODULE_LVL2_ON_SCREEN_MESSAGES.f90
	$(FC) -c Modules/MODULE_LVL2_ON_SCREEN_MESSAGES.f90
MODULE_LVL2_ON_SCREEN_MESSAGES.o: $(MOD_L1) Modules/MODULE_LVL2_ON_SCREEN_MESSAGES.f90
	$(FC) -c Modules/MODULE_LVL2_ON_SCREEN_MESSAGES.f90

#**********************************************************
#**     Edit dependencies of 3rd degree:                 **
#**********************************************************
MODULE_LVL3_MEMORY_MANAGEMENT.mod: $(MOD_L1) $(MOD_L2) MODULE_LVL3_MEMORY_MANAGEMENT.o Modules/MODULE_LVL3_MEMORY_MANAGEMENT.f90
	$(FC) -c Modules/MODULE_LVL3_MEMORY_MANAGEMENT.f90
MODULE_LVL3_MEMORY_MANAGEMENT.o: $(MOD_L1) $(MOD_L2) Modules/MODULE_LVL3_MEMORY_MANAGEMENT.f90
	$(FC) -c Modules/MODULE_LVL3_MEMORY_MANAGEMENT.f90
MODULE_LVL3_READ_OPERATIONS.mod: $(MOD_L1) $(MOD_L2) MODULE_LVL3_READ_OPERATIONS.o Modules/MODULE_LVL3_READ_OPERATIONS.f90
	$(FC) -c Modules/MODULE_LVL3_READ_OPERATIONS.f90
MODULE_LVL3_READ_OPERATIONS.o: $(MOD_L1) $(MOD_L2) Modules/MODULE_LVL3_READ_OPERATIONS.f90
	$(FC) -c Modules/MODULE_LVL3_READ_OPERATIONS.f90
MODULE_LVL3_WRITE_OPERATIONS.mod: $(MOD_L1) $(MOD_L2) MODULE_LVL3_WRITE_OPERATIONS.o Modules/MODULE_LVL3_WRITE_OPERATIONS.f90
	$(FC) -c Modules/MODULE_LVL3_WRITE_OPERATIONS.f90
MODULE_LVL3_WRITE_OPERATIONS.o: $(MOD_L1) $(MOD_L2) Modules/MODULE_LVL3_WRITE_OPERATIONS.f90
	$(FC) -c Modules/MODULE_LVL3_WRITE_OPERATIONS.f90

#**********************************************************
#**     Edit dependencies of 4th degree:                 **
#**********************************************************
MODULE_LVL4_MATRIX_OPERATIONS.mod: $(MOD_L1) $(MOD_L2) $(MOD_L3) MODULE_LVL4_MATRIX_OPERATIONS.o Modules/MODULE_LVL4_MATRIX_OPERATIONS.f90
	$(FC) -c Modules/MODULE_LVL4_MATRIX_OPERATIONS.f90
MODULE_LVL4_MATRIX_OPERATIONS.o: $(MOD_L1) $(MOD_L2) $(MOD_L3) Modules/MODULE_LVL4_MATRIX_OPERATIONS.f90
	$(FC) -c Modules/MODULE_LVL4_MATRIX_OPERATIONS.f90

#**********************************************************
#**     Edit dependencies of 5th degree:                 **
#**********************************************************
MODULE_LVL5_FUNCTIONS.mod: $(MOD_L1) $(MOD_L2) $(MOD_L3) $(MOD_L4) MODULE_LVL5_FUNCTIONS.o Modules/MODULE_LVL5_FUNCTIONS.f90
	$(FC) -c Modules/MODULE_LVL5_FUNCTIONS.f90
MODULE_LVL5_FUNCTIONS.o: $(MOD_L1) $(MOD_L2) $(MOD_L3) $(MOD_L4) Modules/MODULE_LVL5_FUNCTIONS.f90
	$(FC) -c Modules/MODULE_LVL5_FUNCTIONS.f90

#**********************************************************
#**     Edit dependencies of top degree:                 **
#**********************************************************
MODULE_TOP_LASSO.mod: $(MOD_L1) $(MOD_L2) $(MOD_L3) $(MOD_L4) $(MOD_L5) MODULE_TOP_LASSO.o Modules/MODULE_TOP_LASSO.f90
	$(FC) -c Modules/MODULE_TOP_LASSO.f90
MODULE_TOP_LASSO.o: $(MOD_L1) $(MOD_L2) $(MOD_L3) $(MOD_L4) $(MOD_L5) Modules/MODULE_TOP_LASSO.f90
	$(FC) -c Modules/MODULE_TOP_LASSO.f90
MODULE_TOP_GROUP_LASSO.mod: $(MOD_L1) $(MOD_L2) $(MOD_L3) $(MOD_L4) $(MOD_L5) MODULE_TOP_GROUP_LASSO.o Modules/MODULE_TOP_GROUP_LASSO.f90
	$(FC) -c Modules/MODULE_TOP_GROUP_LASSO.f90
MODULE_TOP_GROUP_LASSO.o: $(MOD_L1) $(MOD_L2) $(MOD_L3) $(MOD_L4) $(MOD_L5) Modules/MODULE_TOP_GROUP_LASSO.f90
	$(FC) -c Modules/MODULE_TOP_GROUP_LASSO.f90
MODULE_TOP_SPARSE_GROUP_LASSO.mod: $(MOD_L1) $(MOD_L2) $(MOD_L3) $(MOD_L4) $(MOD_L5) MODULE_TOP_SPARSE_GROUP_LASSO.o Modules/MODULE_TOP_SPARSE_GROUP_LASSO.f90
	$(FC) -c Modules/MODULE_TOP_SPARSE_GROUP_LASSO.f90
MODULE_TOP_SPARSE_GROUP_LASSO.o: $(MOD_L1) $(MOD_L2) $(MOD_L3) $(MOD_L4) $(MOD_L5) Modules/MODULE_TOP_SPARSE_GROUP_LASSO.f90
	$(FC) -c Modules/MODULE_TOP_SPARSE_GROUP_LASSO.f90
MODULE_TOP_MODIFIED_SG_LASSO.mod: $(MOD_L1) $(MOD_L2) $(MOD_L3) $(MOD_L4) $(MOD_L5) MODULE_TOP_MODIFIED_SG_LASSO.o Modules/MODULE_TOP_MODIFIED_SG_LASSO.f90
	$(FC) -c Modules/MODULE_TOP_MODIFIED_SG_LASSO.f90
MODULE_TOP_MODIFIED_SG_LASSO.o: $(MOD_L1) $(MOD_L2) $(MOD_L3) $(MOD_L4) $(MOD_L5) Modules/MODULE_TOP_MODIFIED_SG_LASSO.f90
	$(FC) -c Modules/MODULE_TOP_MODIFIED_SG_LASSO.f90

#**********************************************************
#**     Edit dependencies of highest degree:             **
#**********************************************************
LASSO.o: $(MODS) LASSO.f90
	$(FC) -c LASSO.f90

#**********************************************************
#**     All cleaning instructions:                       **
#**********************************************************
clean:
	rm $(OBJ)
cleanmod:
	rm $(MODS)
cleanexe:
	rm LASSO
cleanall: clean cleanmod
	rm LASSO
