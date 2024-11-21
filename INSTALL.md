# Projet TAS - Installation

Le projet est hébergé sur un [serveur GitLab privé](https://stl.algo-prog.info) du Master STL.
C'est sur ce GitLab que vous trouverez les sources du squelette d'analyseur, que vous créerez le projet git contenant votre analyseur et que ferez votre rendu.

## GitLab STL


### Accès au GitLab STL

Un compte sur le [GitLab STL](https://stl.algo-prog.info) a été créé pour vous par l'enseignant (vous ne vous inscrivez pas vous-même).
Vous devriez avoir reçu un email du serveur intitulé *Account was created for you* avec vos informations de compte, ainsi qu'un deuxième email indiquant que vous avez été ajouté au groupe du cours de cette année. 
Ces emails sont envoyés à votre adresse `@etu.sorbonne-universite.fr`, qui est généralement redirigée vers l'adresse que vous avez renseignée lors de votre inscription pédagogique.
Si vous ne trouvez pas ces emails, pensez à les chercher dans votre boîte spam ou sur le webmail étudiant.
En particulier, si vous utilisez Gmail, les emails  peuvent être répertoriés comme spam et les liens sont alors désactivés (il est nécessaire d'indiquer à Gmail que le courrier n'est pas un spam pour rendre les liens actifs). 

Pour commencer, cliquez sur le lien d'initialisation du mot de passe contenu dans l'email *Account was created for you*.
Si le message est trop vieux, le lien aura expiré. Vous pouvez demander l'envoi d’un nouvel email en cliquant sur *Forgot your password?*.
Après avoir choisi un mot de passe, connectez-vous sur le [serveur GitLab STL](https://stl.algo-prog.info) (attention à ne pas vous connecter à `gitlab.com` de l'entreprise GitLab ; nous utilisons un serveur séparé, dédié à STL).
Notez que :
- votre *username*, utilisé pour la connexion, est votre numéro d'étudiant ;
- votre *email* est celui de l'université `@etu.sorbonne-universite.fr`.

Vous êtes automatiquement membre du groupe `TAS-XXXX`, où `XXXX` indique le semestre en cours (par exemple `2024oct` ou `2024oct-INSTA`).
Ce groupe continent le projet `projet-TAS` en lecture seule contenant un squelette d'analyseur.


### Création d'une copie personnelle du projet sur GitLab (`fork`)

Chaque élève doit faire un fork de ce projet-squelette (qui n'est pas modifiable) pour créer un projet personnel (que vous pouvez compléter).

* Dans le menu à gauche, sélectionnez *Your work* > *Projects* (assurez-vous que votre navigateur est en plein écran pour que le menu ne soit pas masqué). Cliquez, à droite, sur le projet `TAS-XXXX/projet-TAS` (normalement, un seul projet est visible pour vous). `TAS-XXXX` représente le groupe et `projet-TAS` est le nom du projet. Ce projet est visible et commun pour tous les membres du groupe, c'est à dire toute la classe. Il est en lecture seule.
* Faites une copie privée en cliquant sur le bouton *Forks* en haut à droite. Dans *Select a namespace*, sélectionnez votre nom, et cliquez sur *Fork project*. Ceci crée un projet personnel ayant pour nom `prenom.nom/projet-TAS`. Vous travaillerez désormais dans ce projet, et pas dans le groupe `TAS-XXXX`.
* Assurez-vous que vous êtes bien dans votre projet personnel : la barre en haut doit indiquer `prenom.nom > projet-TAS` au lieu de `TAS-XXXX > projet-TAS` (si ce n'est pas le cas, ouvrez le menu *Projects* à gauche et sélectionnez votre projet personnel).
* Sélectionnez *Manage* dans le menu de gauche, puis l'option *Members*. Invitez votre chargé de TME, avec pour rôle `Maintainer`.
C'est important pour qu'il puisse voir votre code.

**Attention** : votre projet sous GitLab STL doit rester _privé_ ; seuls vous et les enseignants doivent pouvoir y accéder, pas les autres élèves.


### Création d'une copie sur votre ordinateur (`clone`)

Pour développer le projet, vous travaillerez avec git sur une copie locale sur votre ordinateur, et vous propagerez régulièrement vos modifications dans votre dépôt sur le serveur GitLab STL.
git vous permet de vous synchroniser entre différents ordinateurs, de garder un historique du développement et de partager votre code avec les enseignants.

Commencez par créer une copie locale (`clone`) du projet personnel sur votre ordinateur avec `git clone URL-du-projet`.
L'URL est donnée par le bouton `Clone` à droite sur la page GitLab du projet. 
Attention à bien faire `clone` du projet personnel et pas du projet du groupe `TAS-XXXX`, en lecture seule. Vous ne pourrez pas propager vos modifications (`git push`) sur ce dernier !


## Installation des dépendances

Les dépendances suivantes doivent être installées sur votre ordinateur pour pouvoir compiler le projet :
* le langage [OCaml :camel:](https://ocaml.org/index.fr.html) (testé avec la version 4.14.0) ;
* [Dune](https://dune.build/) : un système de _build_ pour OCaml ;
* [Menhir](http://gallium.inria.fr/~fpottier/menhir) : un générateur d'analyseurs syntaxiques pour OCaml ;
* [GMP](https://gmplib.org) : une bibliothèque C d'entiers multiprécision (nécessaire pour Zarith et Apron) ;
* [MPFR](http://www.mpfr.org) : une bibliothèque C de flottants multiprécision (nécessaire pour Apron) ;
* [Zarith](http://github.com/ocaml/Zarith/) : une bibliothèque OCaml d'entiers multiprécision ;
* [CamlIDL](http://github.com/xavierleroy/camlidl/) : une bibliothèque OCaml d'interfaçage avec le C ;
* [Apron](https://antoinemine.github.io/Apron/doc/) : une bibliothèque C/OCaml de domaines numériques.


### Installation sous Linux

Sous Ubuntu, Debian et distributions dérivées, l'installation des dépendances peut se faire avec `apt-get` et [opam](https://opam.ocaml.org/) :
```
sudo apt-get update
sudo apt-get install -y build-essential opam libgmp-dev libmpfr-dev git
opam init -y
eval $(opam env)
opam install -y dune menhir zarith mlgmpidl apron
```
Si une des commandes `opam` échoue, essayez de supprimer le répertoire `.opam` et de recommencer en utilisant `opam init -y --disable-sandboxing` au lieu de `opam init -y`.

Les dépendances `apron` et `mlgmpidl` ne sont nécessaires que pour certaines extensions, et vous pouvez les ignorer pour l'instant si elles posent problème.


### Installation sous Windows 10 (ou version supérieure)

Le projet peut également être développé sous Windows en utilisant Windows Subsystem for Linux 2, à condition de posséder une version de Windows 10 ou supérieure.

Les étapes à suivre sont :
- Activer Windows Subsystem for Linux 2 : <https://learn.microsoft.com/fr-fr/windows/wsl/install>
- Installer depuis Microsoft Store la dernière version d'Ubuntu.

Vous pouvez ensuite lancer un shell Ubuntu et entrer les commandes indiquées à la section précédente.


### Autres systèmes

Une solution alternative, mais plus lourde que WSL, est d'installer un système Linux dans une machine virtuelle, e.g., [VirtualBox](https://www.virtualbox.org/).

Il est également possible que le projet fonctionne nativement sous MacOS X.



## Compilation et premiers tests

Après installation des dépendances, tapez la commande suivante sur votre copie locale pour compiler le projet :
```
dune build
``` 

L'exécutable peut ensuite être lancé avec :
```
dune exec -- src/main.exe
```

En cas de succès de la compilation, vous pouvez tester le binaire avec :
1. `dune exec -- src/main.exe tests/01_concrete/0111_rand.c` : cela doit afficher sur la console le texte du programme `tests/01_concrete/0111_rand.c` (en réalité, le programme a été transformé en AST par le *parseur* et reconverti en texte).
2. `dune exec -- src/main.exe tests/01_concrete/0111_rand.c -concrete` : cela doit afficher sur la console l'analyse de toutes les exécutions possibles du programme de test, ici, le fait que `x` vaut une valeur entre 1 et 5.


## La suite

Le fichier [DOC.md](DOC.md) documente les aspects principaux du projet.

Le fichier [TRAVAIL.md](TRAVAIL.md) détaille le travail demandé.
