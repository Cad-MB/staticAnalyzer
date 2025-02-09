# Rapport d'Analyse du Projet d'Interprétation Abstraite

## 1. Introduction et Contexte
Ce projet s'inscrit dans le cadre de l'analyse statique de programmes, en particulier l'interprétation abstraite. L'objectif principal était d'implémenter un analyseur capable de déterminer des propriétés sur les variables d'un programme sans l'exécuter. L'approche s'est concentrée sur deux domaines abstraits : les **intervalles** et les **parités**, ainsi que leur **produit réduit**.

---

## 2. Implémentations Réalisées

### 2.1 Domaine des Intervalles
Le domaine des intervalles a été implémenté en suivant la signature VALUE_DOMAIN. Il prend en charge des bornes infinies et des entiers, ainsi que des opérations fondamentales permettant de gérer les propriétés des variables.  

Les principales fonctionnalités comprennent :  

- **Opérations arithmétiques** : addition, soustraction, multiplication et division avec gestion correcte des cas particuliers.
- **Opérations latticielles** : join, meet, et gestion de l’ordre partiel.
- **Opérateurs de comparaison** : transferts avant et arrière pour assurer la propagation des contraintes.

---

### 2.2 Domaine des Parités
Le domaine des parités permet de raisonner sur les propriétés des entiers en fonction de leur nature paire ou impaire.  

Les principales opérations implémentées sont :  

- **Opérations arithmétiques** : préservation des parités lors des additions et soustractions.  
- **Opérations de réduction** : intersection de valeurs pour affiner la précision.  

---

### 2.3 Produit Réduit
L'implémentation du produit réduit combine les deux domaines précédents afin d'améliorer la précision de l'analyse.  

Elle repose sur :  

- La combinaison des valeurs issues des deux domaines.  
- Un mécanisme de réduction pour éviter des approximations trop grossières.  

---

### 2.4 Analyse Disjonctive
L'analyse des intervalles est imprécise car elle ne représente que des ensembles convexes de valeurs. Pour pallier cette limitation, une **analyse disjonctive** a été intégrée en permettant de raisonner sur des disjonctions d'intervalles.

La solution implémentée repose sur :

- La **représentation de multiples intervalles** pour suivre plusieurs états possibles.
- La **gestion des opérations arithmétiques** en préservant la disjonction.
- La **propagation des contraintes** via les transferts avant et arrière.

Cependant, certains tests démontrent des échecs, analysés ci-dessous.

---

## 3. Tests

### 3.1 Tests Réussis
Les tests validés incluent :  

- **Tests des intervalles** : opérations arithmétiques, comparaisons et assignations.  
- **Tests de comparaison d'intervalles** : précision accrue des opérateurs de comparaison et gestion des cas limites.  
- **Tests du produit réduit** : maintien des propriétés issues des deux domaines combinés.  

---

### 3.2 Tests Échoués et Justifications

1. **Tests de conditionnelles simples (3001, 3008, 3004)**  
   - **Problème** : L'analyse ne préserve pas toutes les alternatives possibles.
   - **Hypothèse** : L'opération `join` fusionne trop rapidement les intervalles sans conserver les disjonctions.
   - **Solution** : Modifier `join` pour préserver explicitement plusieurs intervalles.

2. **Tests de boucles (3002, 3006, 0201, 0202, 0203, 0204)**  
   - **Problème** : Le widening trop agressif cause une perte rapide de précision et des résultats erronés dans les boucles.
   - **Hypothèse** : Le widening fusionne les intervalles trop rapidement sans considérer la structure de boucle et l'unrolling introduit de l'imprécision.
   - **Solution** : Introduire un **widening retardé** qui autorise plus d'itérations avant d'élargir les intervalles et améliorer la gestion des boucles avec retard et déroulement contrôlé.

3. **Tests complexes avec plusieurs variables (3003, 3005, 0207, 0208)**  
   - **Problème** : Les dépendances entre variables ne sont pas bien gérées, ce qui entraîne des approximations incorrectes.
   - **Hypothèse** : Le `meet` pourrait être trop restrictif et éliminer des disjonctions utiles.
   - **Solution** : Améliorer `meet` pour tenir compte des interactions inter-variables et mieux propager les contraintes entre variables interdépendantes.

4. **Test de division par zéro (3009)**  
   - **Problème** : L'analyse ne reconnaît pas que `y` est toujours `0`, causant une mauvaise propagation.
   - **Hypothèse** : Mauvaise gestion des contraintes `if (y != 0)` dans `compare`.
   - **Solution** : Améliorer `compare` pour mieux détecter les contraintes nulles et exclure les valeurs impossibles.

5. **Tests avec assertions et erreurs (0306, 0307, 0309, 0310)**  
   - **Problème** : Des échecs d'assertions liés à une analyse trop large des intervalles.
   - **Hypothèse** : Les intervalles sont trop conservateurs et ne prennent pas en compte les relations numériques entre les variables.
   - **Solution** : Introduire des restrictions supplémentaires pour affiner les bornes et améliorer la propagation des contraintes.

---

## 4. Solutions Proposées

### 4.1 Amélioration de la Préservation des Disjonctions
- Modifier `join` pour conserver **explicitement** des intervalles disjoints.
- Introduire une **fusion intelligente** lorsque les intervalles deviennent trop nombreux.

### 4.2 Widening Progressif
- Appliquer un widening adaptatif avec **seuil d'élargissement progressif**.
- Comptabiliser le **nombre d'itérations** avant de fusionner les intervalles.

### 4.3 Amélioration des Comparaisons
- Raffiner `compare` pour maintenir des **contraintes plus précises** sur les intervalles.
- Gérer plus finement les **conditions `if-else` imbriquées**.

### 4.4 Gestion Améliorée des Boucles
- Tenir compte des **régions de stabilisation** avant widening.
- Intégrer une **propagation plus fine des contraintes inter-iterations**.

---

## 5. Conclusion et Perspectives
Ce projet a permis d'implémenter plusieurs domaines abstraits et leur produit réduit avec succès. L'ajout de l'analyse disjonctive améliore la précision, mais plusieurs problèmes subsistent. Les prochaines étapes incluent :

- **Affiner la gestion des disjonctions** dans `join` et `meet`.
- **Optimiser le widening** pour éviter une perte prématurée de précision.
- **Rendre l'analyse plus robuste** face aux boucles et dépendances inter-variables.

Avec ces améliorations, l'analyseur pourra mieux capturer les propriétés des programmes complexes.
