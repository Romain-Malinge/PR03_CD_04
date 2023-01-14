with Ada.Strings;               use Ada.Strings;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.IO_Exceptions;         use Ada.IO_Exceptions;
with LCA_IP;                    use LCA_IP;
with Prefix_Tree;               use Prefix_Tree;
with Routeur_Exceptions;        use Routeur_Exceptions;
with Routeur_Functions;         use Routeur_Functions;


procedure Test_Lca_IP is

    -- Transformer un String en un Unbounded_String
    function "+" (Item : in String) return Unbounded_String
                  renames To_Unbounded_String;

    -- Afficher une Lca
    procedure Afficher_Lca is new Pour_Chaque (Afficher_Cellule);

    ----------------- Definition parametres --------------------
    Nb_IP : constant Integer := 5; --Nonbre de destination que l'on va ajouter dans la Lca
    Lca : T_LCA_IP; -- La Lca des tests

    -- Les destinations a mettre dans la lca test
    Dest1 : T_Adresse_IP;
    Dest2 : T_Adresse_IP;
    Dest3 : T_Adresse_IP;
    Dest4 : T_Adresse_IP;
    Dest5 : T_Adresse_IP;

    -- Les masques a mettre dans la lca test
    Masque1 : T_Adresse_IP;
    Masque2 : T_Adresse_IP;
    Masque3 : T_Adresse_IP;
    Masque4 : T_Adresse_IP;
    Masque5 : T_Adresse_IP;

    Tab_Dest : array (1..Nb_IP) of T_Adresse_IP;
    Tab_Masques : array (1..Nb_IP) of T_Adresse_IP;
    Tab_Port : constant array (1..Nb_IP) of Unbounded_String := (+"eth1", +"eth2", +"eth3", +"eth4", +"eth5");
    Tab_Freq : constant array (1..Nb_IP) of Integer := (1,3,4,2,13);

    ------Variable test trouver_grand_masque--------
    IP_ajouter : T_Adresse_IP;
    Destination: T_Adresse_IP;
    Grand_Masque: T_Adresse_IP;

    -------------------------- Definition des Test -----------------------------

    procedure Tester_Initialiser (Lca : in out T_LCA_IP) is
    begin
        Initialiser(Lca);
        pragma Assert (Est_Vide(Lca));
        pragma Assert (Taille(Lca) = 0);
    end Tester_Initialiser;

    procedure Tester_Enregistrer (Lca : in out T_LCA_IP) is
    begin
        New_Line;
        Put_Line ("------------------- Test des fonctions --------------------");
        Put_Line ("-- Enregistrer La_Frequence_Premier Destination_Presente --");

        -- Initialisation des destinitaions
        Set_IP(Dest1, 255, 196, 255, 0);
        Set_IP(Dest2, 128, 255, 0, 0);
        Set_IP(Dest3, 128, 128, 0, 0);
        Set_IP(Dest4, 196, 0, 0, 0);
        Set_IP(Dest5, 0, 0, 0, 0);
        Tab_Dest := (Dest1, Dest2, Dest3, Dest4, Dest5);

        -- Initialisation des masques
        Set_IP(Masque1, 255, 255, 255, 0);
        Set_IP(Masque2, 255, 255, 0, 0);
        Set_IP(Masque3, 255, 255, 0, 0);
        Set_IP(Masque4, 255, 0, 0, 0);
        Set_IP(Masque5, 0, 0, 0, 0);
        Tab_Masques := (Masque1, Masque2, Masque3, Masque4, Masque5);

        -- Ajout des destinations
        New_Line;
        for I in 1..Nb_IP loop
            Enregistrer (Lca, Tab_Dest(I), Tab_Masques(I), Tab_Port(I), Tab_Freq(I));
            Put_Line ("AprÃ¨s insertion du quadruplet" & I'Image & " dans la Lca");
            Afficher_Lca(Lca);
            New_Line;
            -- Test l'enregistrement
            pragma Assert (not Est_Vide (Lca));
            pragma Assert (Taille (Lca) = I);
            pragma Assert (Destination_Presente (Lca, Tab_Dest(I)));  --test Destination_PÃ©sente
        end loop;

        pragma Assert ( La_Frequence_Premier(Lca) = 1); -- Test La_Frequence_Premier

        -- Test remplacement si une destination et son masque sont deja prÃ©sents
        Enregistrer(Lca, Tab_Dest(1), Tab_Masques(1), Tab_Port(1), Tab_Freq(1)+5);
        pragma Assert (La_Frequence_Premier(Lca) = 6);
        Put_Line ("AprÃ¨s mofification de la premiÃ¨re frÃ©quence");
        Afficher_Lca(Lca);
    end Tester_Enregistrer;


    procedure Tester_Supprimer_LFU (Lca : in out T_LCA_IP) is
    begin
        New_Line;
        Put_Line("--------------- Test fonction supprimer_LFU ---------------");
        Supprimer_LFU(Lca, Dest5);
        Put_Line ("AprÃ¨s suppression de la frÃ©quence la plus basse");
        Afficher_Lca(Lca);
    end Tester_Supprimer_LFU;


    procedure Tester_Supprimer_1er (Lca : in out T_LCA_IP) is
    begin
        New_Line;
        Put_Line("------------- Test fonction supprimer_premier -------------");
        Supprimer_Premier(Lca);
        Put_Line ("AprÃ¨s suppression du premier element");
        pragma Assert( Taille(Lca) = 4);
        Afficher_Lca(Lca);
    end Tester_Supprimer_1er;


    procedure Tester_Vider (Lca : in out T_LCA_IP) is
    begin
        Vider(Lca);
        pragma Assert (Est_Vide(Lca));
    end Tester_Vider;

    procedure Tester_Trouver_Grand_Masque(Lca : in out T_LCA_IP) is

    begin
        New_Line;
        Put_Line("-------- Test fonction Tester_Trouver_Grand_Masque --------");
        Set_IP(IP_ajouter, 128, 255, 128, 0);
        Set_IP(Grand_Masque, 0, 0, 0, 0);
        Destination := Dest2;
        Trouver_Grand_Masque(Lca,Destination,IP_ajouter, Grand_Masque);
        Put("Le masque à utiliser pour enregistrer l'IP ");
        Put_IP(IP_ajouter); New_Line;
        Put("dans le cache serait : ");
        Put_IP(Grand_Masque);
        New_Line;
        pragma Assert( Grand_Masque = Masque3);
    end Tester_Trouver_Grand_Masque;

    --------------------------- Effectuer les tests -----------------------------
begin
    New_Line;
    Put_Line("----------------------- Test Lca_IP -----------------------");
    Tester_Initialiser(Lca);
    Tester_Enregistrer(Lca); -- Teste les fonctions Enregistrer La_Frequence_Premier Destination_Presente
    Tester_Supprimer_LFU(Lca);
    Tester_Supprimer_1er(Lca);
    Tester_Trouver_Grand_Masque(Lca);
    Tester_Vider(Lca);
    New_Line;
    Put_Line ("Fin des tests : OK");
    New_Line;

end Test_Lca_IP;
