SOMMAIRE:  1\  Liste des fichier dans PR03_Groupe_CD_4
           2\  Utilisation des programmes

/!\ Pour avoir plus d'informations : Lire le Manuel d'utilisation /!\



--------------------------------- Liste des fichier dans pr3 ---------------------------------

 - fichier_cree : dossier où sont placés tous les fichiers créés par GnatStudio pour compiler
 - fichier_exemple : dossier contenant les fichiers pour la démonstration
 - README.txt : explique comment utiliser le programme
 - pr03.grp : fichier grp pour lancer le programme avec GNAT Studio

 - routeur_ll.adb : programme principal modélisant un routeur avec un cache sous forme de Lca
 - routeur_la.adb : programme principal modélisant un routeur avec un cache sous forme d'Arbre

 - routeur_functions.ads : interface du module routeur_functions qui fournit les fonctions utiles aux routeurs
 - routeur_functions.adb : implémentation du module routeur_functions

 - lca_ip.adb : implémentation du module Lca_IP permettant de modéliser une liste chaînée d'IP
 - lca_ip.ads : interface du module lca_ip

 - prefix_tree.adb : implémentation du module Lca_IP permettant de modéliser un arbre binaire d'IP
 - prefix_tree.ads : interface du module lca_ip

 - routeur_exceptions.ads : interface du module routeur_exceptions qui regroupe les exceptions
possibles lors de l'utilisation du routeur

 - test_functions.adb : le programme de test des fonctions utiles pour router les IPs
 - test_functions.adb : le programme de test du module Lca_IP
 - test_functions.adb : le programme de test du module Prefix_Tree



---------------------------------- Utilisation des programmes ----------------------------------

Option 1: Utiliser GnatStudio (Recommandé car plus lisible)
|
|   Ouvrir le fichier pr03.grp avec GNAT Studio et compiler routeur_ll.adb ou routeur_ll.adb
|   Tous les fichiers créés par GnatStudio sont placés dans fichier_cree
|   Placer les fichiers de la table et du paquet dans pr3
|   Le fichier resultat est créé dans pr3
|
|   /!\    Les commandes doivent donc commencer par  ./fichier_cree/routeur_l...    /!\


Option 2: Compilation classique
|
|  Compiler routeur_ll.adb ou routeur_ll.adb
|  Placer les fichiers de la table et du paquet dans pr3
|  Le fichier resultat est créé dans pr3
