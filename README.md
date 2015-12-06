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
* Un typer qui passe 75%.

### Reste à faire

* Faire un fichier misc.ml pour y mettre toutes les fonctions auxiliaires, en  profiter pour transformer `tast.ml` en `tast.mli` ;
* Retirer le constructeur de types `TMbloc`, qui, vu qu'on a traité le sucre  syntaxique des blocs, ne sert plus a rien ;
* Vérifier les conditions d'existence et d'unicité ;
* Faire les vérification de la variance, on a commencé à faire quelque cas,
  qu'il faudra de toute façon retirer mais qui permettent de filtrer certains tests.
* Implémenter la fonction qui vérifie que l'on peut surcharger une méthode. Pour le moment on a un début
de fonction et toutes les idées pour implémenter le reste.

### Difficultés rencontrées

* Un code un peu trop verbeux, dû a notre AST et TAST un poil trop complexe. On  s'est aussi rendu compte un peu trop tard qu'on aurait pu simplifier quelques éléments des AST et TAST en traitant le  sucre syntaxique dans le parseur.
* On s'est apperçu trop tard que l'on avait pas besoin de toute l'information
  sur les objets que l'on gardait dans le contexte. On y conserve beaucoup trop
d'information.
