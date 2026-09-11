#ifndef __TRAITEMENT_H__
#define __TRAITEMENT_H__

#include <stdint.h>
#include <mem.h>
#include <options.h>

extern L_char erreursVersListe(T_tas tas, T_irdata *tgv);
extern void traitementAux(T_tas tasTrt, char *chemin, T_options opts, T_irdata *tgv, T_resultat res);
extern void traitement(char *chemin, T_options opts, T_resultat res);

#endif /* __TRAITEMENT_H__ */
