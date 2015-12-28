Mini-Scala
======

Description
-----------
Ce projet à été réalisé par Martin Pépin et Erkan Narmanli, élèves à l'École normale supérieur dans le cadre du cours de compilation dispensé par Jean-Christophe Filliâtre. Cela consiste en l'implémentation d'un compilateur pour le Mini-Scala, un fragment du langage Scala, dont la description se trouve [sur cette page](https://www.lri.fr/~filliatr/ens/compil/). Le compilateur produit alors un code en assembleur X86-64.

### Choix techniques

* Les environnements qui conservent les classes, variables, contraites de types et les méthodes sont réalisés à partir de listes et non de tables de hachage, ce qui augment un peu le temps d'accès aux champs des éléments qui y sont stockés. 
* Plutôt que d'implémenter un seul abre de syntaxe abstraite (AST) avec des
  champs décorables que nous aurions rempli pendant la phase de typage, nous
avons préféré écrire deux abres de syntaxe asbraite, construits successivement :
un AST lors de l'analyse syntaxique et un AST typé (TAST) lors du typage.

État d'avancement du projet 
-------------
### Pour le moment on a
* Un lexer/parser qui passe tous les tests.
* Un typer qui passe 95% des tests.

### Reste à faire

* Le typeur comporte encore quelques bugs à régler.
* La génération de code.

### Difficultés rencontrées

* Un code un peu trop verbeux, dû a notre AST et TAST un poil trop complexe. On  s'est aussi rendu compte un peu trop tard qu'on aurait pu simplifier quelques éléments des AST et TAST en traitant le  sucre syntaxique dans le parseur. 
* On s'est apperçu trop tard que l'on avait pas besoin de toute l'information
  sur les objets que l'on gardait dans le contexte. On y conserve beaucoup trop
  d'information. Ça a été réglé après mais nous a pas mal ralenti.

### Bugs

* Bug signalé par tobast : cf le test tests/bonus/tobast.scala. Il ne devrait
  pas passer le typage (?) mais le passe quand même. On attend une confirmation
  pour ajouter un test qui rejette ce genre de programme.
* Bug signalé par tobast : il semblerait qu'il faille ajouter address à x86.mli
  (pas encore à ce point dans le projet)
