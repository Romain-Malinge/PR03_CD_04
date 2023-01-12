with Ada.Strings;               use Ada.Strings;	-- pour Both utilisÃÂ© par Trim
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

   --Afficher la lca
    procedure Afficher_Lca is new Pour_Chaque (Afficher_Cellule);


   -----------------definition parametres--------------------
   Nb_IP : constant Integer := 5; --Nonbre de destination que l'on va ajouter dans la Lca

   --les destinations a mettre dans la lca test
   Dest1 : T_Adresse_IP;
   Dest2 : T_Adresse_IP;
   Dest3 : T_Adresse_IP;
   Dest4 : T_Adresse_IP;
   Dest5 : T_Adresse_IP;


   --les masques a mettre dans la lca test
   Masque1 : T_Adresse_IP;
   Masque2 : T_Adresse_IP;
   Masque3 : T_Adresse_IP;
   Masque4 : T_Adresse_IP;
   Masque5 : T_Adresse_IP;

   Tab_Dest : array (1..Nb_IP) of T_Adresse_IP;
   Tab_Masques : array (1..Nb_IP) of T_Adresse_IP;
   Tab_Port : constant array (1..Nb_IP) of Unbounded_String := (+"eth1", +"eth2", +"eth7", +"eth3", +"eth4");
   Tab_Freq : constant array (1..Nb_IP) of Integer := (1,3,4,6,13);

   -----------------definir les tests------------------------
   procedure Construire_Lca_Testeur( Lca : out T_LCA_IP; Bavard: Boolean := False ) is
   begin
      ---initialisation des destinitaions
      Set_IP(Dest1, 128, 255, 0, 0);
      Set_IP(Dest2, 255, 196, 255, 0);
      Set_IP(Dest3, 128, 128, 0, 0);
      Set_IP(Dest4, 196, 0, 0, 0);
      Set_IP(Dest5, 0, 0, 0, 0);
      Tab_Dest := (Dest1, Dest2, Dest3, Dest4, Dest5);

      --initialisation des masques
      Set_IP(Masque1, 255, 255, 0, 0);
      Set_IP(Masque2, 255, 255, 255, 0);
      Set_IP(Masque3, 255, 255, 0, 0);
      Set_IP(Masque4, 255, 0, 0, 0);
      Set_IP(Masque5, 0, 0, 0, 0);
      Tab_Masques := (Masque1,Masque2, Masque3, Masque4, Masque5);


      Initialiser (Lca);
      pragma Assert (Est_Vide (Lca));
      pragma Assert (Taille (Lca) = 0);
      for I in 1..Nb_IP loop
         Enregistrer (Lca, Tab_Dest(I), Tab_Masques(I), Tab_Port(I), Tab_Freq(I));
         if Bavard then
            Put ("Après insertion de l'IP ");
            Put_IP(Tab_Dest(I)); Put(I); New_Line;
            Afficher_Lca(Lca); New_Line;
         else
            null;
         end if;
         --test l'enregistrement
         pragma Assert (not Est_Vide (Lca));
         pragma Assert (Taille (Lca) = I);
         pragma Assert (Destination_Presente (Lca, Tab_Dest(I)));  --test Destination_Pésente

      end loop;
      pragma Assert ( La_Frequence_Premier(Lca) = 1); --test La_Frequence_Premier

      --test remplacement si une destination et son masque sont deja présents
      Enregistrer(Lca, Tab_Dest(1), Tab_Masques(1), Tab_Port(1), Tab_Freq(1)+5);
      pragma Assert (La_Frequence_Premier(Lca) = 6);
      if Bavard then
         Afficher_Lca(Lca);
      else
         null;
      end if;
   end Construire_Lca_Testeur;

   procedure Tester_Construire_Lca_Testeur is
      Lca : T_LCA_IP;
   begin
      Put_Line ("Test des fonctions La_Frequence_Premier,Destination_Pésente et Enregistrer");
      Construire_Lca_Testeur (Lca, True);
      Vider (Lca);
   end Tester_Construire_Lca_Testeur;

   procedure Tester_Supprimer is
      Lca : T_LCA_IP;
   begin
      Put_Line("------------------Test fonction supprimer------------------");
      Construire_Lca_Testeur (Lca, False);
      Supprimer(Lca, Tab_Dest(3));
      Afficher_Lca(Lca);
      Vider (Lca);
   end Tester_Supprimer;

   procedure Tester_Supprimer_1er is
      Lca : T_LCA_IP;
   begin
      Put_Line("--------------Test fonction supprimer_premier--------------");
      Construire_Lca_Testeur (Lca, False);
      Supprimer_Premier(Lca);
      pragma Assert( Taille(Lca) = 4);
      Afficher_Lca(Lca);
      Vider (Lca);
   end Tester_Supprimer_1er;

   procedure Tester_Pour_chaque is
   begin
      Put_Line ("Test pour chaque : ok car la fonction Affiche_Lca qui en est un marche");
   end Tester_Pour_chaque;

   procedure Tester_Trie is
      Lca : T_LCA_IP;
   begin
      Put_Line("--------------------Test fonction Trie--------------------");
      Construire_Lca_Testeur (Lca, False);
      Trie(Lca);
      Afficher_Lca(Lca);
      Vider (Lca);
   end Tester_Trie;

   -----------------effectuer les tests------------------------
begin
   Put_Line("Test Lca");

   Tester_Construire_Lca_Testeur; --test enregistrer+remplacement+La_Frequence_Premier+Destination_Pésente
   Tester_Supprimer;
   Tester_Supprimer_1er;
   Tester_Pour_chaque;
   Tester_Trie;


   Put_Line ("Fin des tests : OK.");
end Test_Lca_IP;
