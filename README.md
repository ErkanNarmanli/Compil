Mini-Scala
======

Description
-----------
Ce projet à été réalisé par Martin Pépin et Erkan Narmanli, élèves à l'École normale supérieur dans le cadre du cours de compilation dispensé par Jean-Christophe Filliâtre. Cela consiste en l'implémentation d'un compilateur pour le Mini-Scala, un fragment du langage Scala, dont la description se trouve [sur cette page](https://www.lri.fr/~filliatr/ens/compil/). Le compilateur produit alors un code en assembleur x86-64.

### Choix techniques

* Les environnements qui conservent les classes, variables, contraites de types
  et les méthodes sont réalisés à partir de listes.
* Plutôt que d'implémenter un seul abre de syntaxe abstraite (AST) avec des
  champs décorables que nous aurions rempli pendant la phase de typage, nous
  avons préféré écrire deux abres de syntaxe asbraite, construits successivement :
  un AST lors de l'analyse syntaxique et un AST typé (TAST) lors du typage.
* Les substitutions sont des Map, qui associent un type (Tast.typerType) à un 
  identificateur (ident = string).
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

État d'avancement du projet 
-------------
### Pour le moment on a
* Un lexer/parser qui passe tous les tests.
* Un typer qui passe tous les tests.

### Reste à faire

* La génération de code.

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

### Bugs

* Bug signalé par tobast : il semblerait qu'il faille ajouter address à x86.mli
  (pas encore à ce point dans le projet)


