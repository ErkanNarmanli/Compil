Mini-Scala
======

Description
-----------
Ce projet à été réalisé par Martin Pépin et Erkan Narmanli, élèves à l'École
normale supérieur dans le cadre du cours de compilation dispensé par
Jean-Christophe Filliâtre. Cela consiste en l'implémentation d'un compilateur
pour le Mini-Scala, un fragment du langage Scala, dont la description se
trouve [sur cette page](https://www.lri.fr/~filliatr/ens/compil/).
Le compilateur produit un code en assembleur x86-64.

### Choix techniques

#### Dans le typeur

* Les environnements qui conservent les classes, variables, contraites de types
  et les méthodes sont réalisés à partir de listes.
* Plutôt que d'implémenter un seul abre de syntaxe abstraite (AST) avec des
  champs décorables que nous aurions rempli pendant la phase de typage, nous
  avons préféré écrire deux abres de syntaxe asbraite, construits successivement :
  un AST lors de l'analyse syntaxique et un AST typé (TAST) lors du typage.
* Les substitutions sont des Map, qui associent un type (Tast.typerType) à un 
  identificateur (`ident` = `string`).
* On a un environnement global qui connait toutes les classes du programme.
* Chaque classe a son propre environnement avec
  * Les classes qu'elle connait en fonction de sa position dans le programme.
  * Ses variables et méthodes ainsi que celles dont elle a hérité le cas échéant.
  * Les constraintes liées aux bornes sur les paramètres de type.
* Chaque méthode a son propre environnement avec 
  * Les classes qu'elle connait.
  * Ses variables et méthodes ainsi que celles de la classes qui sont déclarées
    avant elle.
  * Les contraintes liées aux bornes sur les paramètres de type de la classe
    dans laquelle elle est déclarée.

#### Dans le compilateur

* On a utilisé les tables de hachage préconisées par le sujet.
* En plus des tables de hachages préconisées par le sujet, on a rajouté une
  tables qui associe à un nom de classe et un nom de méthode une étiquette
	unique. Cela permet d'éviter de se faire piéger par certains noms de classes
  (cf. tests/exec\_add/good/ident.scala)
* On a toujours accès à l'environnement local d'une classe pendant la
  compilation.
* Lors de la compilation d'une expression, on connait :
  * La hauteur de la pile.
  * La position, relativement à %rbp des variables locales.
  * Le nom de la classe qu'on est en train de typer.
* Les arguments des méthodes sont passés sur la pile.
* On utilise peu de registres, en particulier, seul les registres %rbp et %rbx
  sont callee-saved. On a pas vraiment eu besoin de plus, ne faisant aucune
  optimisation.
* On s'est permis de commenter une fonction dans `x86_64.ml` pour éviter un
  warning et d'ajouter une ligne pour `address` dans `x86_64.mli`.

État d'avancement du projet 
-------------
### Pour le moment on a
* Un lexer/parser qui passe tous les tests.
* Un typer qui passe tous les tests.
* Un compilateur qui passe tous les tests

### Reste à faire
* A priori c'est terminé !
* Pour aller plus loin, on pourrait faire quelques optimisations bien que ce ne
  soit pas l'objectif du sujet.

### Difficultés rencontrées

* Un code un peu trop verbeux, dû a notre AST et TAST un poil trop complexe.
  * L'arbre a été simplifié en cours de route.
* On  s'est aussi rendu compte un peu trop tard qu'on pouvait simplifier
  quelques éléments des AST et TAST, notamment en traitant lesucre syntaxique
  dans le parseur. 
  * Cela a été fait plus tard.
* On s'est apperçu que l'on avait pas besoin de toute l'information
  sur les objets que l'on gardait dans le contexte. On y conserve beaucoup trop
  d'information.
  * Ça a été réglé après.
* Des petites correction a posteriori dans le typeur pour réordonner ou nettoyer
  des listes de variables/méthodes dans lesquelles certaines valeur
  apparaissaient plusieurs fois.
* La génération de code s'est bien passée à partir du moment où on a utilisé à
  fond les `call\_star`.
  * Des erreurs de postion des champs par moments.
  * Des registres pollués par moment sans qu'on s'en apperçoive, c'est corrigé.


