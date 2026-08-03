###################################
#            Variables            #
###################################

##################################################
# Generic variables
##################################################

GCC=gcc
MUSL_HOME?=/usr/local/musl
OPTIM_FLAG?=

##################################################
# Tax computation configuration
##################################################

MPP_FUNCTION_BACKEND?=enchainement_primitif
MPP_FUNCTION?=enchainement_primitif_interpreteur
SOURCE_EXT_DIR=$(ROOT_DIR)/m_ext/$(YEAR)
REPO?=ir

# Paramètres pour les millésimes
ifeq ($(REPO),svn)
  ifeq ($(filter x$(MODE), xc xcorr xcorrectif), x$(MODE))
    # 2025 37674
    SOURCE_FILES?=$(call source_dir_sans_cibles_m,$(ROOT_DIR)/ir-calcul/M_SVN/$(YEAR).corr/code_m/)
    SOURCE_EXT_FILES?=\
      $(SOURCE_EXT_DIR)/cibles.m \
      $(SOURCE_EXT_DIR)/primitif.m \
      $(SOURCE_EXT_DIR)/cibles_corr.m \
      $(SOURCE_EXT_DIR)/codes_1731.m \
      $(SOURCE_EXT_DIR)/commence_par_5.m \
      $(SOURCE_EXT_DIR)/commence_par_7.m \
      $(SOURCE_EXT_DIR)/commence_par_H.m \
      $(SOURCE_EXT_DIR)/correctif.m \
      $(SOURCE_EXT_DIR)/main_corr.m
  else
    # 2025 3.11 37626
    SOURCE_FILES?=$(call source_dir_sans_cibles_m,$(ROOT_DIR)/ir-calcul/M_SVN/$(YEAR)/code_m/)
    SOURCE_EXT_FILES?=\
      $(SOURCE_EXT_DIR)/cibles.m \
      $(SOURCE_EXT_DIR)/primitif.m \
      $(SOURCE_EXT_DIR)/main.m
  endif
else ifeq ($(filter $(YEAR), 2022 2023 2024), $(YEAR))
  SOURCE_FILES?=$(call source_dir_sans_cibles_m,$(ROOT_DIR)/ir-calcul/sources$(YEAR)*/)
  ifeq ($(filter x$(MODE), xc xcorr xcorrectif), x$(MODE))
    SOURCE_EXT_FILES?=\
      $(SOURCE_EXT_DIR)/cibles.m \
      $(SOURCE_EXT_DIR)/primitif.m \
      $(SOURCE_EXT_DIR)/cibles_corr.m \
      $(SOURCE_EXT_DIR)/codes_1731.m \
      $(SOURCE_EXT_DIR)/commence_par_5.m \
      $(SOURCE_EXT_DIR)/commence_par_7.m \
      $(SOURCE_EXT_DIR)/commence_par_H.m \
      $(SOURCE_EXT_DIR)/correctif.m \
      $(SOURCE_EXT_DIR)/main_corr.m
  else
    SOURCE_EXT_FILES?=\
      $(SOURCE_EXT_DIR)/cibles.m \
      $(SOURCE_EXT_DIR)/primitif.m \
      $(SOURCE_EXT_DIR)/main.m
  endif
else ifeq ($(filter $(YEAR), 2019 2020 2021), $(YEAR))
  SOURCE_FILES?=$(call source_dir,$(ROOT_DIR)/ir-calcul/sources$(YEAR)*/)
  SOURCE_EXT_FILES?=$(call source_dir_ext,$(SOURCE_EXT_DIR))
else ifeq ($(filter $(YEAR), 2018), $(YEAR))
  SOURCE_FILES?=$(call source_dir,$(ROOT_DIR)/ir-calcul/sources2018m_6_7*/)
  SOURCE_EXT_FILES?=$(call source_dir_ext,$(SOURCE_EXT_DIR))
else ifeq ($(filter $(YEAR), 0), $(YEAR))
  SOURCE_FILES?=
  SOURCE_EXT_FILES?=$(call source_dir_ext,$(SOURCE_EXT_DIR))
else
  $(warning ATTENTION: auncune configuration trouvée pour l'année $(YEAR))
endif

# Paramètres pour les tests millésimés
TEST_VAR_DEFS=
ifeq ($(filter $(YEAR), 2025), $(YEAR))
  # 2025 3.11 37626
  TESTS_DIR?=$(ROOT_DIR)/tests/$(YEAR)/fuzzing
else ifeq ($(filter $(YEAR), 2024), $(YEAR))
  # 2024 3.13 34996
  TEST_VAR_DEFS=-D ANCSDED=2026 -D V_MILLESIME=defaut
  TESTS_DIR?=$(ROOT_DIR)/tests/$(YEAR)/fuzzing
else ifeq ($(filter $(YEAR), 2018 2019 2020 2022 2023), $(YEAR))
  # 2023 8.0 33095
  # 2022 6.1 29657
  # 2021 5.7 27852, tests fuzzés manquants
  # 2020 6.5 25513
  # 2019 8.0 22543
  # 2018 06.7 19515, ne compile pas
  # 2018 6.3 18591, ne compile pas
  TESTS_DIR?=$(ROOT_DIR)/tests/$(YEAR)/fuzzing
else ifeq ($(filter $(YEAR), 0), $(YEAR))
  TESTS_DIR?=$(ROOT_DIR)/tests/$(YEAR)
else
  $(warning ATTENTION: aucun test défini pour l'année $(YEAR))
  TESTS_DIR?=$(ROOT_DIR)/tests/$(YEAR)
endif

##################################################
# Mlang configuration
##################################################

MLANG_BIN=dune exec mlang --

PRECISION?=double
MLANG_DEFAULT_OPTS=\
 -A iliad\
 --display_time --debug\
 --precision $(PRECISION)

##################################################
# C backend configuration
##################################################

# CC is a GNU make default variable defined to CC
# It so can't be overriden by conditional operator ?=
# We check the origin of CC value to not override CL argument or explicit environment.
ifeq ($(origin CC),default)
  CC=gcc
endif

# Options pour le compilateur C
# Attention, très long à compiler avec GCC en O2/O3
COMMON_CFLAGS?=-std=c89 -pedantic -Werror=uninitialized
ifdef OPTIM_FLAG
  COMPILER_SPECIFIC_CFLAGS=-O$(OPTIM_FLAG)
endif

BACKEND_CFLAGS?=$(COMMON_CFLAGS) $(COMPILER_SPECIFIC_CFLAGS)

# Directory of the driver sources for tax calculator
DRIVER_DIR?=c_driver
# Driver sources for tax calculator
DRIVER_H_FILES?=aide.h chaine.h commun.h fichiers.h format.h ida.h irj.h liste.h mem.h options.h traitement.h utils.h completion.h
DRIVER_C_FILES?=aide.c chaine.c commun.c fichiers.c format.c ida.c irdata.c irj.c liste.c main.c mem.c options.c traitement.c utils.c completion.c
DRIVER_FILES?=$(DRIVER_H_FILES) $(DRIVER_C_FILES)

# Flag to disable binary dump comparison
NO_BINARY_COMPARE?=1

##################################################
# Etc.
##################################################

ifeq ($(CODE_COVERAGE), 1)
  CODE_COVERAGE_FLAG=--code_coverage
else
  CODE_COVERAGE_FLAG=
endif

ifeq ($(TEST_FILTER), 1)
  TEST_FILTER_FLAG=--dgfip_test_filter
  TEST_FILES=$(TESTS_DIR)/[A-Z]*
else
  TEST_FILTER_FLAG=
  TEST_FILES=$(TESTS_DIR)/*
endif

# Précision des comparaisons entre flottants pendant les calculs
COMPARISON_ERROR_MARGIN?=0.000001

MLANG_INTERPRETER_OPTS=\
  --income-year=$(YEAR) \
  --comparison_error_margin=$(COMPARISON_ERROR_MARGIN) \
  --mpp_function=$(MPP_FUNCTION)

MLANG_TEST=$(MLANG_BIN) $(MLANG_DEFAULT_OPTS) $(MLANG_INTERPRETER_OPTS) $(CODE_COVERAGE_FLAG) $(TEST_VAR_DEFS)

DGFIP_DIR?=examples/dgfip_c/ml_primitif

MAKE_DGFIP=$(MAKE) --no-print-directory -f $(ROOT_DIR)/Makefile -C $(ROOT_DIR)/$(DGFIP_DIR) ROOT_DIR="$(ROOT_DIR)"

MAKE_DGFIP_CALC=$(MAKE) --no-print-directory -f $(ROOT_DIR)/Makefile -C $(ROOT_DIR)/$(DGFIP_DIR)/calc ROOT_DIR="$(ROOT_DIR)"

IRJ_BIN=irj_checker
IRJ_TESTS_DIRS?=tests/2019 tests/2020 tests/2022 tests/2023

INTERP_PROGRESS=examples/dgfip_c/ml_primitif/.interpreter_progress
MLANG_HASH=examples/dgfip_c/ml_primitif/.mlang.hash
