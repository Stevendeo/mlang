  $ mlang domain.m --mpp_function test --income-year=2020 --dgfip_options="-m2020,-X" --backend dgfip_c --output output/enchain.c -A app > /dev/null
  $ cat output/m_domain.c
  #include "mlang.h" 
  struct S_discord * test(T_irdata* irdata) {
    int sav35_nb_tmps_target = irdata->nb_tmps_target;
    int sav35_nb_refs_target = irdata->nb_refs_target;
    char *def_calculee = irdata->def_calculee;
    double *calculee = irdata->calculee;
  
    irdata->refs_org = irdata->refs_org + 1;
    irdata->nb_tmps_target = 0;
    irdata->nb_refs_target = 1;
    {
      char cond36_def;
      double cond36_val;
      {
         int vardef0;
         T_varinfo* varval0;
         {
           vardef0 = 1;
           varval0 = IR_((-1)/*VAR*/);
         }
         {
            int cond_def0;
            double cond_val0;
            {
              char res40_def;
              double res40_val;
              res40_def = dans_domaine(varval0, 7.0, &res40_def, &res40_val);
              cond_def0 = 1;
              cond_val0 = res40_val;
            }
            /* definedness check */
            if(cond_def0 == 0) {cond36_def = 0; cond36_val = 0.0;}
            /* if true */
            else if (NEQ_E(cond_val0,0.0)) {
            
              {
                cond36_def = 1;
                cond36_val = 1.0;
              }
              /* if false */
              } else {
            
              {
                 int cond_def1;
                 double cond_val1;
                 {
                   char res39_def;
                   double res39_val;
                   res39_def =
                     dans_domaine(varval0, 8.0, &res39_def, &res39_val);
                   cond_def1 = 1;
                   cond_val1 = res39_val;
                 }
                 /* definedness check */
                 if(cond_def1 == 0) {cond36_def = 0; cond36_val = 0.0;}
                 /* if true */
                 else if (NEQ_E(cond_val1,0.0)) {
                 
                   {
                     cond36_def = 1;
                     cond36_val = 1.0;
                   }
                   /* if false */
                   } else {
                 
                   {
                     cond36_def = 1;
                     cond36_val = 0.0;
                   }
                   }
                 }
              }
            }
         }
      if (cond36_def && cond36_val != 0.0) {
        {
          def_calculee[0/*Z*/] = 1;
          calculee[0/*Z*/] = 1.0;
        }
      }
    }
  
    irdata->refs_org = irdata->refs_org - 1;
    irdata->nb_refs_target = sav35_nb_refs_target;
    irdata->nb_tmps_target = sav35_nb_tmps_target;
    return irdata->discords;
  }
  
  $ gcc -c output/*.c -Ioutput -lm -DTARGET=test
  $ gcc *.o -Ioutput -o ./cal -lm
  $ ./cal -mode primitif test.irj
  IACT003 | tests IRJ
  DLDC002 | année par défaut (année revenu + 1: 2021)
  IACT005 | traitement du fichier "test.irj"
  IACT006 | "test.irj" OK
  IACT009 | 1/1 fichier correct
  IACT010 | 0/1 fichiers incorrects
  IACT011 | 0/1 fichiers invalides

  $ rm output/m_domain.* m_domain.o
  $ mlang domain_undef.m --mpp_function test --income-year=2020 --dgfip_options="-m2020,-X" --backend dgfip_c --output output/enchain.c -A app > /dev/null
  $ cat output/m_domain_undef.c
  #include "mlang.h" 
  struct S_discord * test(T_irdata* irdata) {
    int sav35_nb_tmps_target = irdata->nb_tmps_target;
    int sav35_nb_refs_target = irdata->nb_refs_target;
    char *def_calculee = irdata->def_calculee;
    double *calculee = irdata->calculee;
  
    irdata->nb_tmps_target = 0;
    irdata->nb_refs_target = 0;
    {
      char cond36_def;
      double cond36_val;
      {
         int vardef0;
         T_varinfo* varval0;
         {
           vardef0 = def_calculee[0/*I*/];
           if (vardef0) {
             varval0 =
               lis_tabaccess_varinfo(irdata, 0, def_calculee[0/*I*/], calculee[0/*I*/]);
           } else varval0 = NULL;
         }
         {
            int cond_def0;
            double cond_val0;
            {
              char res40_def;
              double res40_val;
              res40_def = dans_domaine(varval0, 7.0, &res40_def, &res40_val);
              cond_def0 = 1;
              cond_val0 = res40_val;
            }
            /* definedness check */
            if(cond_def0 == 0) {cond36_def = 0; cond36_val = 0.0;}
            /* if true */
            else if (NEQ_E(cond_val0,0.0)) {
            
              {
                cond36_def = 1;
                cond36_val = 1.0;
              }
              /* if false */
              } else {
            
              {
                 int cond_def1;
                 double cond_val1;
                 {
                   char res39_def;
                   double res39_val;
                   res39_def =
                     dans_domaine(varval0, 8.0, &res39_def, &res39_val);
                   cond_def1 = 1;
                   cond_val1 = res39_val;
                 }
                 /* definedness check */
                 if(cond_def1 == 0) {cond36_def = 0; cond36_val = 0.0;}
                 /* if true */
                 else if (NEQ_E(cond_val1,0.0)) {
                 
                   {
                     cond36_def = 1;
                     cond36_val = 1.0;
                   }
                   /* if false */
                   } else {
                 
                   {
                     cond36_def = 1;
                     cond36_val = 0.0;
                   }
                   }
                 }
              }
            }
         }
      if (cond36_def && cond36_val != 0.0) {
        {
          def_calculee[5/*Z*/] = 1;
          calculee[5/*Z*/] = 1.0;
        }
      }
    }
  
    irdata->nb_refs_target = sav35_nb_refs_target;
    irdata->nb_tmps_target = sav35_nb_tmps_target;
    return irdata->discords;
  }
  
  $ gcc -c output/*.c -Ioutput -lm -DTARGET=test
  $ gcc *.o -Ioutput -o ./cal -lm
  $ ./cal -mode primitif test.irj
  IACT003 | tests IRJ
  DLDC002 | année par défaut (année revenu + 1: 2021)
  IACT005 | traitement du fichier "test.irj"
  IACT006 | "test.irj" OK
  IACT009 | 1/1 fichier correct
  IACT010 | 0/1 fichiers incorrects
  IACT011 | 0/1 fichiers invalides

  $ rm output/m_domain_undef.c m_domain_undef.o
  $ mlang domain_evt.m --mpp_function test --income-year=2020 --dgfip_options="-m2020,-X" --backend dgfip_c --output output/enchain.c -A app > /dev/null
  $ cat output/m_domain_evt.c
  #include "mlang.h" 
  struct S_discord * test(T_irdata* irdata) {
    int sav35_nb_tmps_target = irdata->nb_tmps_target;
    int sav35_nb_refs_target = irdata->nb_refs_target;
    char *def_calculee = irdata->def_calculee;
    double *calculee = irdata->calculee;
  
    irdata->nb_tmps_target = 0;
    irdata->nb_refs_target = 0;
    {
      char cond36_def;
      double cond36_val;
      {
         int vardef0;
         T_varinfo* varval0;
         {
           vardef0 = def_calculee[0/*I*/];
           if (vardef0) {
             varval0 =
               event_field_code_var(irdata, def_calculee[0/*I*/], calculee[0/*I*/]);
           } else varval0 = NULL;
         }
         {
            int cond_def0;
            double cond_val0;
            {
              char res40_def;
              double res40_val;
              res40_def = dans_domaine(varval0, 7.0, &res40_def, &res40_val);
              cond_def0 = 1;
              cond_val0 = res40_val;
            }
            /* definedness check */
            if(cond_def0 == 0) {cond36_def = 0; cond36_val = 0.0;}
            /* if true */
            else if (NEQ_E(cond_val0,0.0)) {
            
              {
                cond36_def = 1;
                cond36_val = 1.0;
              }
              /* if false */
              } else {
            
              {
                 int cond_def1;
                 double cond_val1;
                 {
                   char res39_def;
                   double res39_val;
                   res39_def =
                     dans_domaine(varval0, 8.0, &res39_def, &res39_val);
                   cond_def1 = 1;
                   cond_val1 = res39_val;
                 }
                 /* definedness check */
                 if(cond_def1 == 0) {cond36_def = 0; cond36_val = 0.0;}
                 /* if true */
                 else if (NEQ_E(cond_val1,0.0)) {
                 
                   {
                     cond36_def = 1;
                     cond36_val = 1.0;
                   }
                   /* if false */
                   } else {
                 
                   {
                     cond36_def = 1;
                     cond36_val = 0.0;
                   }
                   }
                 }
              }
            }
         }
      if (cond36_def && cond36_val != 0.0) {
        {
          def_calculee[1/*Z*/] = 1;
          calculee[1/*Z*/] = 1.0;
        }
      }
    }
  
    irdata->nb_refs_target = sav35_nb_refs_target;
    irdata->nb_tmps_target = sav35_nb_tmps_target;
    return irdata->discords;
  }
  
  $ gcc -c output/*.c -Ioutput -lm -DTARGET=test
  $ gcc *.o -Ioutput -o ./cal -lm
  $ ./cal -mode primitif test.irj
  IACT003 | tests IRJ
  DLDC002 | année par défaut (année revenu + 1: 2021)
  IACT005 | traitement du fichier "test.irj"
  IACT006 | "test.irj" OK
  IACT009 | 1/1 fichier correct
  IACT010 | 0/1 fichiers incorrects
  IACT011 | 0/1 fichiers invalides
  $ rm output/m_domain_evt.c m_domain_evt.o


  $ mlang multimax.m --mpp_function test --income-year=2020 --dgfip_options="-m2020,-X" --backend dgfip_c --output output/enchain.c -A app > /dev/null
  $ cat output/m_multimax.c
  #include "mlang.h" 
  struct S_discord * test(T_irdata* irdata) {
    int sav35_nb_tmps_target = irdata->nb_tmps_target;
    int sav35_nb_refs_target = irdata->nb_refs_target;
    char *def_calculee = irdata->def_calculee;
    double *calculee = irdata->calculee;
  
    irdata->nb_tmps_target = 0;
    irdata->nb_refs_target = 0;
    {
      char res36_def;
      double res36_val;
      res36_def =
        multimax_varinfo(irdata, (irdata->var_space), I_(calculee,0/*TAB*/), 1,
          2.0, &res36_def, &res36_val);
      def_calculee[4/*Z*/] = res36_def;
      if (def_calculee[4/*Z*/]) {
        calculee[4/*Z*/] = res36_val;
      } else calculee[4/*Z*/] = 0.0;
    }
  
    irdata->nb_refs_target = sav35_nb_refs_target;
    irdata->nb_tmps_target = sav35_nb_tmps_target;
    return irdata->discords;
  }
  
  $ gcc -c output/*.c -Ioutput -lm -DTARGET=test
  $ gcc *.o -Ioutput -o ./cal -lm
  $ ./cal -mode primitif test.irj
  IACT003 | tests IRJ
  DLDC002 | année par défaut (année revenu + 1: 2021)
  IACT005 | traitement du fichier "test.irj"
  IACT006 | "test.irj" OK
  IACT009 | 1/1 fichier correct
  IACT010 | 0/1 fichiers incorrects
  IACT011 | 0/1 fichiers invalides
