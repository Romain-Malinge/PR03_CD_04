SOMMAIRE:  1\  Liste des fichier dans PR03_Groupe_CD_4
           2\  Utilisation des programmes

/!\ Pour avoir plus d'informations : Lire le Manuel d'utilisation /!\



--------------------------- Liste des fichier dans PR03_Groupe_CD_4 ---------------------------

 - fichier_cree : docier ou sont placés tout les fichiers créés par GnatStudio pour compiler
 - README.txt : explique comment utiliser le programme
 - pr03.grp : fichier grp pour lancer le programe avec GNAT Studio

 - routeur_ll.adb : programme principal modélisant un routeur avec un cache sous forme de Lca
 - routeur_la.adb : programme principal modélisant un routeur avec un cache sous forme d'Arbre

 - routeur_functions.ads : interface du module routeur_functions qui fournie les fonction utile aux routeurs
 - routeur_functions.adb : implémentation du module routeur_functions

 - lca_ip.adb : implémentation du module Lca_IP permettant de modéliser une liste chainé d'IP
 - lca_ip.ads : interface du module lca_ip

 - prefix_tree.adb : implémentation du module Lca_IP permettant de modéliser une arbre binaire d'IP
 - prefix_tree.ads : interface du module lca_ip

 - routeur_exceptions.ads : interface du module routeur_exceptions qui regroupe les exceptions
possibles lors de l'utilisation du routeur

 - test_functions.adb : le programme de teste des fonctions utiles pour router les IP
 - test_functions.adb : le programme de teste du module Lca_IP
 - test_functions.adb : le programme de teste du module Prefix_Tree



---------------------------------- Utilisation des programmes ----------------------------------

Option 1: Utiliser GnatStudio (Recomendé car plus lisible)
|
|   Ouvrir le fichier pr03.grp avec GNAT Studio et compiler routeur_ll.adb ou routeur_ll.adb
|   Tout les fichiers créés par GnatStudio sont placer dans fichier_cree
|   Placer les fichiers de la table et du paquet dans PR03_Groupe_CD_4
|   Le fichier resultat est créé dans PR03_Groupe_CD_4
|
|   /!\    Les commandes doivent donc commencer par  ./fichier_cree/routeur_l...    /!\


Option 2: Compilation classique
|
|  Compiler routeur_ll.adb ou routeur_ll.adb
|  Placer les fichiers de la table et du paquet dans PR03_Groupe_CD_4
|  Le fichier resultat est créé dans PR03_Groupe_CD_4