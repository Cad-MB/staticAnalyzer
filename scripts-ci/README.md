# Intégration continue

## Image Docker

L'image docker contient un environnement minimal permettant de compiler et de tester le projet TAS.
Elle est basée sur une Debian stable et y ajoute OCaml et les dépendances du projet.

Pour créer l'image docker, faire dans ce répertoire :
```
docker build -t tas .
```

Pour tester l'image :
```
docker images
docker run tas
```


## Scripts de tests

Ces scripts son lancés dans l'image docker par gitlab-ci.
Voir le fichier de configuration [../.gitlab-ci.yml](../.gitlab-ci.yml).

```
run.sh
```

