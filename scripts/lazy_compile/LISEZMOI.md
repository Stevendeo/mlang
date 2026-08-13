# Compilation paresseuse

## Comment compiler

Le script `build.sh` compile le script et génère le binaire `lcc`.

## Utilisation

Lancez `lcc` avec:
- `-F` le dossier contenant les fichiers C à compiler;
- `-C` le fichier de configuration.

Cing variables d'environnement peuvent également être redéfinies:

- `OUTPUT_DIR` est le dossier où seront générés les fichiers .o (par defaut: `"output"`);
- `DEPGRAPH_FILENAME` est le fichier contenant le graphe de dépendance (ce fichier est utilisé pour stocker les résultats de compilation) (par defaut: `".depgraph"`);
- `DEBUG` permet d'afficher des messages de debuggage (default: `0`);
- `PEDANTIC` rend le compilateur utilisé pédant (default: `1`);
- `CC` est le compilateur utilisé (default: `gcc`)

### Fichier de configuration

Le fichier de configuration commence par `#External dependencies` et est
suivi d'un ensemble de lignes sous le format suivant :

```
<fichier>:<commande>
```

où: 

- le fichier est une des dépendances externes du projet;
- la commande sera lancée par le script pour avoir la version de la dépendance externe
  (si ce n'est pas nécessaire, mettez `echo ""`).

Le fichier `config` est un exemple.
