# Projet TAS - Documentation

Nous décrivons succinctement ici les fichiers et répertoires principaux fournis pour le projet, ainsi que la syntaxe du langage d'entrée et les options d'analyse.


## Architecture du projet

L'arborescence des sources est la suivante :
* [src/main.ml](src/main.ml) : point d'entrée de l'analyseur, à modifier pour ajouter des nouvelles analyses et des options ;
* [src/libs/](src/libs) : contient une version légèrement améliorée du module Map d'OCaml ;
* [src/frontend/](src/frontend) : répertoire de l'analyse syntaxique (transformation du source texte en AST) ;
* [src/frontend/abstract_syntax_tree.ml](src/frontend/abstract_syntax_tree.ml) : type des arbres syntaxiques abstraits (AST) ;
* [src/frontend/lexer.mll](src/frontend/lexer.mll) : analyseur lexical OCamlLex ;
* [src/frontend/parser.mly](src/frontend/parser.mly) : analyseur syntaxique Menhir ;
* [src/frontend/file_parser.ml](src/frontend/file_parser.ml) : point d'entrée pour la transformation du source en AST ;
* [src/frontend/abstract_syntax_printer.ml](src/frontend/abstract_syntax_printer.ml) : affichage d'un AST sous forme de code source ;
* [src/domains/](src/domains) : répertoire des domaines d'interprétation de la sémantique ;
* [src/domains/domain.ml](src/domains/domain.ml) : signature des domaines représentant des ensembles d'environnements ;
* [src/domains/concrete_domain.ml](src/domains/concrete_domain.ml) : domaine concret de la sémantique collectrice ;
* [src/domains/value_domain.ml](src/domains/value_domain.ml) : signature des domaines représentant des ensembles d'entiers ;
* [src/domains/constant_domain.ml](src/domains/constant_domain.ml) : exemple de domaine d'ensembles d'entiers : le domaine des constantes ;
* [src/domains/non_relational_domain.ml](src/domains/non_relational_domain.ml) : foncteur qui crée un domaine d'environnements à partir d'un domaine d'entiers, en associant à chaque variable une valeur du domaine d'entiers ;
* [src/interpreter/interpreter.ml](src/interpreter/interpreter.ml) : interprète générique de programmes, paramétré par un domaine d'environnements ;
* [tests/](tests) : ensemble de programmes dans le langage analysé pour tester votre analyseur.


## Langage

Nous décrivons succinctement les traits du langage d'entrée de l'analyseur :
* les tests :

```
if (bexpr) { block }
if (bexpr) { block } else { block }
```

* les boucles :

```
while (bexpr) { block }
```

* les affectations :

```
var = expr
```

* l'affichage de la valeur des variables spécifiées :

```
print(var1,...,varn)
```

* l'affichage de l'environnement complet (toutes les variables) :

```
print_all
```

* l'arrêt du programme :

```
halt
```

* les assertions, qui arrêtent le programme sur un message d'erreur si la condition booléenne n'est pas vérifiée :

```
assert(bexpr)
```

* les expressions entières `expr` sont composées des opérateurs classiques `+`, `-`, `*`, `/`, des variables, des constantes, plus une opération particulière, `rand (l,h)`, où l et h sont deux entiers, et qui représente l'ensemble des entiers entre l et h ;
* les expressions booléennes `bexpr` utilisées dans les tests et les boucles sont composées des opérateurs `&&`, `||`, `!`, des constantes `true` et `false`, et de la comparaison de deux expressions entières grâce aux opérateurs `<`, `<=`, `>`, `>=`, `==`, `!=` ;
* les blocs sont composés d'une suite de déclarations de variables, suivie d'une suite d'instructions :

```
{ decl1; ...; declN; stat1; ...; statM; }
```

Seul le type `int` est reconnu et les déclarations n'ont pas d'initialisation (il faut faire suivre d'une affectation dans une instruction séparée).
Une déclaration ne déclare qu'une variable à la fois (`int a; int b;` est possible, mais pas `int a,b;`).
Dans un bloc, toutes les déclarations doivent précéder toutes les instructions.

Un exemple simple de programme valide est :
```
{
  int x;
  x = 2 + 2;
  print(x);
}
```

Pour plus d'informations sur la syntaxe, vous pouvez consulter le fichier d'analyse syntaxique [src/frontend/parser.mly](src/frontend/parser.mly).
Vous trouverez également des exemples de programmes dans le répertoire [tests/](tests).


## Options de l'analyseur

Quelques options sont disponibles en ligne de commande, à vous d'en ajouter :
1. `-concrete` indique qu'il faut exécuter le programme dans la sémantique concrète collectrice ;
2. `-trace` permet de suivre le déroulement des calculs en affichant l'environnement après l'exécution de chaque instruction.
3. `-nonreldebug` permet de suivre plus précisément le déroulement des calculs des analyses non-relationnelles en traçant l'évaluation de chaque opérateur arithmétique abstrait du domaine de valeurs.
