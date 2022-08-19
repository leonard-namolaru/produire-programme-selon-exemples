# Projet UE Programmation Fonctionnelle Avancée (Université Paris Cité, M1) - Programmation par l’exemple
Concat est un petit langage de programmation qui permet d’accomplir des taches simples de
transformations de chaınes de caracteres. 

Au lieu de faire écrire un programme Concat par un programmeur, un utilisateur donne quelques exemples d’une entrée et de la sortie attendue du programme. 
Puis, le programme va produire un programme qui est cohérent avec les exemples fournis (ce projet va construire le programme Concat sous la forme d’une syntaxe abstraite).

## Le programme accepte les modes suivants de fonctionnement :
1. `getconcat <fichier>` afficher le programme produit à partir du contenu de `<fichier>`.
2. `getconcat <fichier> <fichier>` doit créer un programme à partir du contenu du premier fichier (contenant des lignes "input output") 
  puis utilise ce programme sur le second fichier. Celui-ci ne contiendra que des lignes "input", et ce programme affiche chaque "output" calculé correspondant.
