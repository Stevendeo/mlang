# compir

cible regle_1:
application: iliad;
BIDON = 1;
APPLI_BATCH = 0;
APPLI_ILIAD = 1;

cible calcul_primitif:
application: iliad;
calculer domaine primitive;

cible calcul_primitif_isf:
application: iliad;
calculer domaine isf;

cible calcul_primitif_taux:
application: iliad;
calculer domaine taux;

cible calcul_correctif:
application: iliad;
calculer domaine corrective;

cible sauve_base_1728:
application: iliad;
calculer domaine base_1728 corrective;

cible sauve_base_premier:
application: iliad;
calculer domaine base_premier corrective;

cible sauve_base_stratemajo:
application: iliad;
calculer domaine base_stratemajo corrective;

cible sauve_base_anterieure:
application: iliad;
calculer domaine base_anterieure corrective;

cible sauve_base_anterieure_cor:
application: iliad;
calculer domaine base_anterieure_cor corrective;

cible sauve_base_inr_tl:
application: iliad;
calculer domaine base_inr_tl corrective;

cible sauve_base_inr_tl22:
application: iliad;
calculer domaine base_inr_tl22 corrective;

cible sauve_base_inr_tl24:
application: iliad;
calculer domaine base_inr_tl24 corrective;

cible sauve_base_inr_ntl:
application: iliad;
calculer domaine base_inr_ntl corrective;

cible sauve_base_inr_ntl22:
application: iliad;
calculer domaine base_inr_ntl22 corrective;

cible sauve_base_inr_ntl24:
application: iliad;
calculer domaine base_inr_ntl24 corrective;

cible sauve_base_inr_ref:
application: iliad;
calculer domaine base_inr_ref corrective;

cible sauve_base_inr_r9901:
application: iliad;
calculer domaine base_inr_r9901 corrective;

cible sauve_base_inr_intertl:
application: iliad;
calculer domaine base_inr_intertl corrective;

cible sauve_base_inr_inter22:
application: iliad;
calculer domaine base_inr_inter22 corrective;

cible sauve_base_inr_cimr99:
application: iliad;
calculer domaine base_inr_cimr99 corrective;

cible sauve_base_inr_cimr07:
application: iliad;
calculer domaine base_inr_cimr07 corrective;

cible sauve_base_inr_cimr24:
application: iliad;
calculer domaine base_inr_cimr24 corrective;

cible sauve_base_inr_tlcimr07:
application: iliad;
calculer domaine base_inr_tlcimr07 corrective;

cible sauve_base_inr_tlcimr24:
application: iliad;
calculer domaine base_inr_tlcimr24 corrective;

cible sauve_base_tlnunv:
application: iliad;
calculer domaine base_TLNUNV corrective;

cible sauve_base_tl:
application: iliad;
calculer domaine base_tl corrective;

cible sauve_base_tl_init:
application: iliad;
calculer domaine base_tl_init corrective;

cible sauve_base_tl_rect:
application: iliad;
calculer domaine base_tl_rect corrective;

cible sauve_base_initial:
application: iliad;
calculer domaine base_INITIAL corrective;

cible sauve_base_abat98:
application: iliad;
calculer domaine base_ABAT98 corrective;

cible sauve_base_abat99:
application: iliad;
calculer domaine base_ABAT99 corrective;

cible sauve_base_majo:
application: iliad;
calculer domaine base_MAJO corrective;

cible sauve_base_inr:
application: iliad;
calculer domaine base_INR corrective;

cible sauve_base_HR:
application: iliad;
calculer domaine base_HR corrective;

cible sauve_base_primitive_penalisee:
application: iliad;
calculer domaine base_primitive_penalisee corrective;

cible ENCH_TL:
application: iliad;
calculer enchaineur ENCH_TL;

cible verif_calcul_primitive_isf:
application: iliad;
nettoie_erreurs;
verifier domaine isf : avec nb_categorie(calculee *) > 0;

cible verif_calcul_primitive:
application: iliad;
calculer cible verif_calcul_primitive_isf;
si nb_bloquantes() = 0 alors
  verifier domaine primitive
  : avec
      nb_categorie(calculee *) > 0
      ou numero_verif() = 1021;
finsi

cible verif_calcul_corrective:
application: iliad;
nettoie_erreurs;
calculer cible calcul_primitif_isf;
calculer cible verif_calcul_primitive_isf;
si nb_bloquantes() = 0 alors
  verifier domaine corrective
  : avec
      nb_categorie(calculee *) > 0
      ou numero_verif() = 1021;
finsi

cible verif_saisie_cohe_primitive_isf_raw:
application: iliad;
nettoie_erreurs;
verifier domaine isf
: avec nb_categorie(saisie *) > 0 et nb_categorie(calculee *) = 0;

cible verif_saisie_cohe_primitive:
application: iliad;
nettoie_erreurs;
calculer cible verif_saisie_cohe_primitive_isf_raw;
si nb_bloquantes() = 0 alors
  calculer cible calcul_primitif_isf;
  calculer cible verif_calcul_primitive_isf;
  si nb_bloquantes() = 0 alors
    verifier domaine primitive
    : avec
        nb_categorie(saisie *) > 0 et nb_categorie(calculee *) = 0
        et numero_verif() != 1021;
  finsi
finsi

cible verif_saisie_cohe_corrective:
application: iliad;
nettoie_erreurs;
calculer cible verif_saisie_cohe_primitive_isf_raw;
si nb_bloquantes() = 0 alors
  verifier domaine corrective
  : avec
      nb_categorie(saisie *) > 0 et nb_categorie(calculee *) = 0
      et numero_verif() != 1021;
finsi

cible verif_cohe_horizontale:
application: iliad;
nettoie_erreurs;
verifier domaine horizontale corrective;

cible verif_contexte_cohe_primitive:
application: iliad;
nettoie_erreurs;
verifier domaine primitive
: avec nb_categorie(saisie contexte) = nb_categorie(*);

cible verif_contexte_cohe_corrective:
application: iliad;
nettoie_erreurs;
verifier domaine corrective
: avec nb_categorie(saisie contexte) = nb_categorie(*);

cible verif_famille_cohe_primitive:
application: iliad;
nettoie_erreurs;
verifier domaine primitive
: avec
    nb_categorie(saisie famille) > 0
    et nb_categorie(*) = nb_categorie(saisie famille) + nb_categorie(saisie contexte)
    et numero_verif() != 1021;

cible verif_famille_cohe_corrective:
application: iliad;
nettoie_erreurs;
verifier domaine corrective
: avec
    nb_categorie(saisie famille) > 0
    et nb_categorie(*) = nb_categorie(saisie famille) + nb_categorie(saisie contexte)
    et numero_verif() != 1021;

cible verif_revenu_cohe_primitive:
application: iliad;
nettoie_erreurs;
verifier domaine primitive
: avec nb_categorie(saisie revenu) > 0 et nb_categorie(calculee *) = 0;

cible verif_revenu_cohe_corrective:
application: iliad;
nettoie_erreurs;
verifier domaine corrective
: avec nb_categorie(saisie revenu) > 0 et nb_categorie(calculee *) = 0;

