with Prefix_Tree;           use Prefix_Tree;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Routeur_Functions;     use Routeur_Functions;
with LCA_IP;                use LCA_IP;


procedure Test_Prefix_Tree is

    -- Faciliter la transformation String ->  Unbouded_String
    function "+" (Item : in String) return Unbounded_String
                   renames To_Unbounded_String;

    ------------------------- Définition des variables -------------------------
    Arbre : T_Arbre;              -- Arbre sur lequel on effectuera les tests
    IP : T_Adresse_IP;            -- Variable qui va contenir les IP que l'on va ajouter Ã  Arbre
    Masque : T_Adresse_IP;        -- Variable qui va contenir les masques que l'on va ajouter Ã  Arbre
    Port : Unbounded_String;      -- Variable qui va contenir les interfaces que l'on va ajouter Ã  Arbre
    Avancement : Integer;         -- Variable entiÃ¨re utile pour la rÃ©cursivitÃ© de la procÃ©dure Enregistrer du module Prefix_Tree
    Min : Integer;                -- Variable utile Ã  la supression de la feuille de plus petit rang
    Destination : T_Adresse_IP;   -- Variable contenant l'IP de la destination demandée
    Frequence : Integer;          -- Variable contenant la fréquence de la destination demandée
    Verif_Masque : T_Adresse_IP;  -- Variable pour vérifier si le masque est bien le bon
    
begin

    -- Initialisation des variables (pour éviter les warning de non initialisation)
    Initialiser(Arbre);
    IP := 0;
    Masque := 0;
    Frequence := 0;
    Verif_Masque := 0;
    Destination := 0;
    
    New_Line;
    Put_Line("------------------- Teste Enregistrer -------------------");
    -- 1ère feuille
    Set_IP(IP, 192, 0 , 0, 0); -- 1 1 0 0 0 0 0 0
    Set_IP(Masque, 255, 0, 0, 0);
    Port := +"eth3";
    Avancement := 0;
    -- Selon la logique de conception, l'arbre Ã©tant vide, la feuille est crÃ©Ã©e juste Ã  droite de la racine
    Enregistrer(Arbre, IP, Masque, Port, 3, 0 , Avancement);
    
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
    Port := +"eth1";
    Avancement := 0;
    -- Selon la logique de conception, cela ne devrait pousser aucune feuille et créer une feuille à gauche de la racine
    Enregistrer(Arbre, IP, Masque, Port, 1, 0 , Avancement);
    Put_Line("Après ajout des 3 feuilles");
    Afficher_Arbre(Arbre);
    
    -- On rafraichit le masque, le rang et la fréquence de la 3ème feuille 
    Avancement := 0;
    Set_IP(Masque, 255, 255, 0, 0);
    Enregistrer(Arbre, IP, Masque, Port, 1, 1, Avancement);
    
    Put_Line("Après actualisation de eth1");
    Afficher_Arbre(Arbre);
    
    -- Teste de la fonction Taille 
    pragma Assert (Taille(Arbre) = 3);
    Put_Line("La taille de l'arbre est bien 3");
    
    
    -- Supprimer la feuille avec le rang le plus petit
    New_Line;
    Put_Line("--------------------- Teste Supprimer --------------------");
    Min := 10;
    Least_ranked (Arbre, IP, Min);
    pragma Assert (Min = 1);
    Put_Line("La feuille de plus petit rang vaut" & Min'Image &" c'est celle contenant l'interface eth1");
    Supprimer_Destination (Arbre, IP);
    Put_Line("Après supprétion de eth1");
    Afficher_Arbre (Arbre);
    pragma Assert (Taille(Arbre) = 2);
    Put_Line ("L'arbre a bien reduit de taille, il ne reste plus que" & Taille(Arbre)'Image & " feuilles");
    Min := 10;
    Least_ranked (Arbre, IP, Min);
    pragma Assert (Min = 2);
    Put_Line("La feuille de plus petit rang est maintenant celle de eth2");
    
     
    -- Comparer une IP dans l'Arbre
    New_Line;
    Put_Line("---------------- Teste Comparer et Vider-----------------");
     
    -- Si la feuille existe dans l'arbre
    Afficher_Arbre(Arbre);
    Set_IP(IP, 128, 0 ,0 ,0);
    Set_IP(Verif_Masque, 255, 0, 0, 0);
    Masque := 0;
    Comparer_Arbre (Arbre, IP, Destination, Masque, Port, Frequence);
    pragma Assert ((IP and Masque) = Destination);
    pragma Assert (Verif_Masque = Masque);
    pragma Assert (Port = +"eth2");
    pragma Assert (Frequence = 0);
    Put_Line ("La feuille (128.0.0.0) a bien été trouvée et les données ont bien été mise a jour");
     
    -- Si la feuille n'existe pas dans l'arbre (celle qui a été supprimée récemment)
    Set_IP(IP, 96, 0, 0, 0);
    Set_IP(Verif_Masque, 255, 255, 0, 0);
    Comparer_Arbre (Arbre, IP, Destination, Masque, Port, Frequence);
    pragma Assert ((IP and Masque) /= Destination);
    pragma Assert (Verif_Masque /= Masque);
    pragma assert (Port /= +"eth1");
    pragma assert (Frequence /= 1);
    Put_Line ("Aucune feuille d'IP 96.0.0.0 a été trouvée dans l'arbre, les variables n'ont pas été actualisées");
     
    -- Fin du test
    Vider(Arbre);
    pragma Assert (Est_Vide(Arbre));
    New_Line;
    Put_Line ("L'arbre a bien été vidée.");
    
    New_Line;
    Put_Line("Fin des tests : OK");
    New_Line;
end Test_Prefix_Tree;
