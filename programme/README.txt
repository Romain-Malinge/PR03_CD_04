----------------------------------- Utilisation du programme -----------------------------------


Fichiers compris dans le zip :

 - obj : docier on sont créer tout les fichiers utile à gnat pour compiler saut resultat.txt
qui est toujours créer dans le docier Grp_CD_04
 - README.txt : explique comment utiliser le programme
 - routeur.adb : programme principal modélisant le routeur sans cache
 - routeur_functions.adb : implémentation du module routeur_functions qui permet de regrouper
les sous-programmes utile à la modélisation du routeur
 - routeur_functions.ads : interface du module routeur_functions
 - routeur_exceptions.ads : interface du module routeur_exceptions qui regroupe les exceptions
possibles lors de l'utilisation du routeur
 - lca_ip.adb : implémentation du module lca_ip permettant de modéliser une structure de donnée
associative (SDA) d'IP
 - lca_ip.ads : interface du module lca_ip
 - Paquet.txt : fichier texte regroupant les paquets à envoyer grâce à la table de routage
 - Table.txt : fichier texte au sein duquel il y a la table de routage
 - pr03.grp : fichier grp pour lancer le programe avec GNAT Studio


Utiliser le programme : 

Pour utiliser le programme, il faut compiler routeur.adb et lancer l'executable. Ou utiliser le
fichier pr03.grp avec GNAT Studio.
Ainsi les paquets du fichier texte paquet.txt est envoyé selon la table de routage de table.txt
Les résultats seront écrits dans un fichier texte nommé resultats.txt créé par le programme
lui-même dans le docier Grp_CD_04.
