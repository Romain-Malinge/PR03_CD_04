--------------------- Utilisation du programme ---------------------

Fichiers compris dans le zip : 

 - routeur.adb : programme principal modélisant le routeur simple 
 - routeur_functions.adb : implémentation du module routeur_functions qui permet de regrouper les sous-programmes utile à la modélisation du routeur
 - routeur_functions.ads : interface du module routeur_functions
 - routeur_exceptions.ads : interface du module routeur_exceptions qui regroupe les exceptions possibles lors de l'utilisation du routeur
 - lca_ip.adb : implémentation du module lca_ip permettant de modéliser une structure de donnée associative (SDA) d'IP
 - lca_ip.ads : interface du module lca_ip
 - paquet.txt : fichier texte regroupant les paquets à envoyer grâce à la table de routage
 - table.txt : fichier texte au sein duquel il y a la table de routage (pour une quasi-même adresse ip ayant un masque plus long, seul celui qui a le masque le plus long sera enregistré dans la table)
 
Utiliser le programme : 

Pour utiliser le programme, il faut compiler routeur.adb et lancer l'executable. Ainsi les paquets du fichier texte paquet.txt est envoyé selon la table de routage de table.txt
Les résultats seront écrits dans un fichier texte nommé resultats.txt créé par le programme lui-même.