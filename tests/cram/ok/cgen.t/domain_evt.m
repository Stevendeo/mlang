application app;
variable saisie : attribut mon_attribut;
Z : calculee restituee primrest = 0 : "";
I : calculee primrest = 1 : "";

cible test:
application : app;
si dans_domaine(champ_evenement(I, code), calculee *)
alors
  Z = 1;
finsi
