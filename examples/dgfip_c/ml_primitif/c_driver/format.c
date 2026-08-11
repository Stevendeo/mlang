
#include <utils.h>
#include <liste.h>
#include <chaine.h>
#include <options.h>
#include <irj.h>
#include <fichiers.h>
#include <ida.h>
#include <format.h>

#include <mlang.h>

void verifieFormat(char *chemin, T_options opts, T_resultat res) {
  T_tas tasFmt = NULL;
  T_fich fich = NULL;
  T_irj irj = NULL;
  int code = IRJ_CODE_VIDE;
  L_char lnom = NULL;
  char *nom = NULL;
  int result;

  res->ok = 1;
  res->temps_ms = 0;
  tasFmt = memCreeTas();
  fich = ouvreFich(tasFmt, chemin);
  if (fich == NULL) {
    discoFichier(chemin, -1);
    res->ok = -1;
    goto fin;
  }
  irj = creeIrj(tasFmt, opts->args.fmt.strict);
  code = codeIrj(irj);
  lnom = NIL(char);
  while (code != IRJ_FIN && code != IRJ_INVALIDE) {
    lisIrj(fich, irj);
    code = codeIrj(irj);
    switch (code) {
      case IRJ_NOM:
        lnom = CONS(tasFmt, char, strCopie(tasFmt, irj->args.nom), lnom);
        break;
      case IRJ_NOM_FIN:
        RETOURNE(char, &lnom, lnom);
        nom = strConcatListe(tasFmt, lnom);
        break;
      case IRJ_DEF_VAR:
        switch (irj->section) {
          case IRJ_ENTREES_PRIMITIF_DEBUT:
            if (cherche_varinfo_statique(irj->args.defVar.var) == NULL) {
              anoVarAbs(irj->args.defVar.var);
              res->ok = -1;
              goto fin;
            }
            break;
          case IRJ_RESULTATS_PRIMITIF_DEBUT:
          case IRJ_RESULTATS_CORRECTIF_DEBUT:
          case IRJ_RESULTATS_RAPPELS_DEBUT:
            if (opts->args.fmt.strict) {
              T_varinfo *varinfo = cherche_varinfo_statique(irj->args.defVar.var);

              if (varinfo == NULL) {
                anoVarAbs(irj->args.defVar.var);
                res->ok = -1;
                goto fin;
              } else if (! varinfo->est_restituee) {
                anoVarNonRestituee(irj->args.defVar.var);
                res->ok = -1;
                goto fin;
              }
            }
            break;
          default:
            res->ok = -1;
            goto fin;
        }
        break;
      case IRJ_DEF_ANO:
        switch (irj->section) {
          case IRJ_CONTROLES_PRIMITIF_DEBUT:
          case IRJ_CONTROLES_CORRECTIF_DEBUT:
          case IRJ_CONTROLES_RAPPELS_DEBUT:
            break;
          default:
            res->ok = -1;
            goto fin;
        }
        break;
      case IRJ_DEF_RAP:
        if (irj->section == IRJ_ENTREES_RAPPELS_DEBUT) {
          if (cherche_varinfo_statique(irj->args.defRap.code) == NULL) {
            anoVarAbs(irj->args.defRap.code);
            res->ok = -1;
            goto fin;
          }
        } else {
          res->ok = -1; 
          goto fin;
        }
        break;
      case IRJ_INVALIDE:
        anoLigneInvalide(irj->ligne, irj->args.err);
        res->ok = -1;
        goto fin;
      case IRJ_CODE_VIDE:
        anoCodeVide(irj->ligne);
        res->ok = -1;
        goto fin;
      default:
        break;
    }
  }

fin:
  memLibere(nom);
  fermeFich(fich);
  memLibereTas(tasFmt);
  return;
}

