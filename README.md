# PFA Laboratoire - Parseur

## Objectifs

- Créer un parseur capable de construire un arbre syntaxique abstrait à partir de la syntaxe suivante:

  - terme = facteur + terme | facteur
  - facteur = expression simple * facteur | expression simple
  - expression simple = entier | terme parenthésé
  - entier = [0..9]+
  - terme parenthésé = (terme)

- Ajouter la notion de variable à la syntaxe (définition et utilisation)
- Ajouter une fonction d'évaluation des expressions dans le style applicatif
