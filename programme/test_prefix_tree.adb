with Prefix_Tree;           use Prefix_Tree;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Routeur_Functions;     use Routeur_Functions;
with LCA_IP;                use LCA_IP;

procedure Test_Prefix_Tree is

     -- Faciliter la transformation String ->  Unbouded_String
     function "+" (Item : in String) return Unbounded_String
                   renames To_Unbounded_String;

     -- DÃ©finition des variables
     Arbre : T_Arbre;            -- Arbre sur lequel on effectuera les tests
     IP : T_Adresse_IP;          -- Variable qui va contenir les IP que l'on va ajouter Ã  Arbre
     Masque : T_Adresse_IP;      -- Variable qui va contenir les masques que l'on va ajouter Ã  Arbre
     Port : Unbounded_String;    -- Variable qui va contenir les interfaces que l'on va ajouter Ã  Arbre
     Avancement : Integer;       -- Variable entiÃ¨re utile pour la rÃ©cursivitÃ© de la procÃ©dure Enregistrer du module Prefix_Tree
     Min : Integer;              -- Variable utile Ã  la supression de la feuille de plus petit rang
     Destination : T_Adresse_IP; -- Variable contenant l'IP de la destination demandée
     Frequence : Integer;        -- Variable contenant la fréquence de la destination demandée
     Verif_Masque : T_Adresse_IP;-- Variable pour vérifier si le masque est bien le bon
begin

     -- Initialisation des variables
     Initialiser(Arbre);
     IP := 0;         --   \
     Masque := 0;      --   \
     Frequence := 0;    --  => Afin d'éviter l' "erreur" de non initialisation
     Verif_Masque := 0; --  /
     Destination := 0; --  /
     
     -- Enregistrer des Ã©lÃ©ments dans l'arbre
     -- 1ère feuille
     Set_IP(IP, 192, 0 , 0, 0); -- 1 1 0 0 0 0 0 0
     Set_IP(Masque, 255, 0, 0, 0);
     Port := +"eth1";
     Avancement := 0;
     -- Selon la logique de conception, l'arbre Ã©tant vide, la feuille est crÃ©Ã©e juste Ã  droite de la racine
     Enregistrer(Arbre, IP, Masque, Port, 1, 0 , Avancement);

     -- 2ème feuille
     Set_IP(IP, 128, 0, 0, 0); -- 1 0 0 0 0 0 0 0
     Set_IP(Masque, 255, 0, 0, 0);
     Port := +"eth2";
     Avancement := 0;
     -- Selon la logique de conception, cela devrait pousser la feuille prÃ©cÃ©dente pour pouvoir discriminer la nouvelle feuille
     Enregistrer(Arbre, IP, Masque, Port, 2, 0 , Avancement);

     -- 3ème feuille
     Set_IP(IP, 96, 0, 0, 0); -- 0 1 0 0 0 0 0 0
     Set_IP(Masque, 255, 0, 0, 0);
     Port := +"eth3";
     Avancement := 0;
     -- Selon la logique de conception, cela ne devrait pousser aucune feuille et créer une feuille à gauche de la racine
     Enregistrer(Arbre, IP, Masque, Port, 3, 0 , Avancement);
     -- On rafraichit le masque, le rang et la fréquence de la 3ème feuille 
     Avancement := 0;
     Set_Ip(Masque, 255, 255, 0, 0);
     Enregistrer(Arbre, IP, Masque, Port, 5, 1, Avancement);
     
     Afficher_Arbre(Arbre);
     
     -- Supprimer la feuille avec le rang le plus petit
     Min := Taille(Arbre);
     pragma Assert ( Min = 3 ) ;
     Put_Line("La taille de l'arbre est bien" & Min'Image &".");
     
     Least_ranked (Arbre, IP, Min);
     pragma Assert ( Min = 1 ) ; 
     Put_Line("La feuille de plus petit rang vaut" & Min'Image &".");
     Supprimer_Destination (Arbre, IP);
     
     Min := Taille(Arbre);
     pragma Assert ( Min = 2 );
     Put_Line ("L'arbre a bien reduit de taille, il ne reste plus que" & Min'Image & "feuilles.");
     Least_ranked (Arbre, IP, Min);
     pragma Assert ( Min = 2 );
     Put_Line("La feuille de plus petit rang vaut désormais" & Min'Image &".");
     Afficher_Arbre (Arbre);
     
     -- Comparer une IP dans l'Arbre
     
     -- Si la feuille existe dans l'arbre
     Set_IP(IP, 96, 0 ,0 ,0);
     Set_IP(Verif_Masque, 255, 255, 0, 0);
     Comparer_Arbre (Arbre, IP, Destination, Masque, Port, Frequence);
     pragma Assert (IP = Destination);
     pragma Assert (Verif_Masque = Masque);
     pragma Assert (Port = +"eth3");
     pragma Assert (Frequence = 1);
     Put_Line ("La feuille (96.0.0.0) a bien été trouvée et les données ont bien été exfiltrées de la feuille.");
     
     -- Si la feuille n'existe pas dans l'arbre (celle qui a été supprimée récemment)
     Set_IP(IP, 192, 0, 0, 0);
     Set_IP(Verif_Masque, 255, 0, 0, 0);
     Comparer_Arbre (Arbre, IP, Destination, Masque, Port, Frequence);
     pragma Assert (not (IP = Destination));
     pragma Assert (not (Verif_Masque = Masque));
     pragma assert (not (Port = +"eth1"));
     pragma assert (not (Frequence = 0));
     Put_Line ("Aucune feuille d'IP 192.0.0.0 a été trouvée dans l'arbre, les variables n'ont pas été actualisées.");
     
     -- Fin du test
     Vider(Arbre);
     pragma Assert ( Est_Vide(Arbre) );
     Put_Line ("L'arbre a bien été vidée.");
     Put_Line("Tous les tests sont passés.");
end Test_Prefix_Tree;
