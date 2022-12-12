with Ada.Strings;               use Ada.Strings;	-- pour Both utilisÃ© par Trim
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Exceptions;            use Ada.Exceptions;	-- pour Exception_Message
with LCA_IP;                    use LCA_IP;
with Routeur_Exceptions;        use Routeur_Exceptions;
with Routeur_Functions;         use Routeur_Functions;


procedure Routeur is

     ------------------------------- PROCEDURES ---------------------------------

     UN_OCTET : constant T_Adresse_IP := 2 ** 8;

     -- Afficher une Lca.
     procedure Afficher_Lca is
     new Pour_Chaque (Afficher_Cellule);

     -- Afficher une Lca avec des titres.
     procedure Afficher_Lca_Titre (Lca : in T_LCA_IP; Titre : Unbounded_String) is
     begin
          New_Line;
          Put_Line ("--------------------------- " & Titre & " ----------------------------");
          Put_line ("Destination         Masque              Interface    FrÃ©quence");
          Afficher_Lca (Lca);
          New_Line;
     end Afficher_Lca_Titre;

     -- Transformer un String en un Unbounded_String
     function "+" (Item : in String) return Unbounded_String
                  renames To_Unbounded_String;

     -- Transformer un Unbounded_String en un String
     function "-" (Item : in Unbounded_String) return String
                  renames To_String;


     --------------------- Variables globales du programme ----------------------
     F_Table : File_Type;
     F_Paquet : File_Type;
     F_Resultat : File_Type;
     Fichier : File_Type;
     Table : T_LCA_IP;
     IP : T_Adresse_IP;
     N1, N2, N3, N4 : Integer;
     Entier : Integer;
     Caractere : Character;
     Port : Unbounded_String;
     Masque : T_Adresse_IP;

     Nom_Paquet : Unbounded_String;
     Nom_Table : Unbounded_String;
     Nom_Resultat : Unbounded_String;
     Taille_Cache : Integer;
     Politique : Unbounded_String;
     Fin : Boolean;
     Bavard : Boolean;
     Nbr_Ajoute : Integer;


begin

     -- Initialisation des variables
     Nom_Paquet := +"paquet.txt";
     Nom_Table := +"table.txt";
     Nom_Resultat := +"resultats.txt";
     Taille_Cache := 10;
     Politique := +"FIFO";
     Bavard := True;
     Fin := False;
     Nbr_Ajoute := 0;


     -- Traiter les arguments de la ligne de commande
     for a in 1..Argument_Count loop
          -- Traiter le a Ã©me argument de la ligne de commande
          if Argument(a)(1) = '-' then
               case Argument(a)(2) is
                    when 'c' => Taille_Cache := Integer'Value (Argument(a+1));
                    when 'P' => Politique := +Argument(a+1);
                    when 'S' => Bavard := False;
                    when 'p' => Nom_Paquet := +Argument(a+1);
                    when 't' => Nom_Table := +Argument(a+1);
                    when 'r' => Nom_Resultat := +Argument(a+1);
                    when others => null;
               end case;
          else
               null;
          end if;
     end loop;

     Open (Fichier, In_File, "Table.txt");
     Get (Fichier, N1);
     Put (N1);
     IP := T_Adresse_IP(N1);
     Afficher_IP(IP);


     -- Traiter les exceptions
     if Taille_Cache < 1 then
          raise Taille_Cache_Exception;

     elsif not txt_present(Nom_Paquet) then
          raise Not_Txt_Exception;

     elsif not txt_present(Nom_Table) then
          raise Not_Txt_Exception;

     elsif not txt_present(Nom_Resultat) then
          raise Not_Txt_Exception;

     elsif (Politique /= "FIFO" and Politique /= "LRU" and Politique/= "LFU") then
          raise Not_Politique_Exception;
     end if;

     ---------------------- Initialiser le cache (à faire plus tard) -------------------------

     -- Gestion des fichiers

     Open (F_Paquet, In_File, - Nom_Paquet);
     Create (F_Resultat, Out_File, - Nom_Resultat);
     Open (F_Table , In_File, - Nom_Table);

     Initialiser(Table);
     while not( End_Of_File (F_Table) ) loop



          Get(F_Table, Entier);
          IP := T_Adresse_IP(Entier);
          for i in 1..3 loop
		Get(F_Table, Caractere);
		Get(F_Table, Entier);
		IP :=  IP * UN_OCTET + T_Adresse_IP(Entier);
          end loop;

          -- Convertir le Masque venant du fichier Table

          Get(F_Table, Entier);
          Masque := T_Adresse_IP(Entier);
          for i in 1..3 loop
		Get(F_Table, Caractere);
		Get(F_Table, Entier);
		Masque :=  Masque * UN_OCTET + T_Adresse_IP(Entier);
          end loop;

          -- Récupérer le port associé à l'IP du fichier Table

          Get_Line (F_Table, Port);
          Trim (Port, Both);

          Enregistrer (Table, IP, Masque, Port, 0);
     end loop;
     Close (F_Table);


     -- Teste temporaire de Lca
     Initialiser (Table);
     N1 := 110; N2 := 120; N3 := 130; N4 := 140;
     --Set_IP(IP, N1, N2, N3, N4);
     Enregistrer(Table, IP, IP, +"eth1", 1000);
     N1 := 150; N2 := 160; N3 := 170; N4 := 180;
     --Set_IP(IP, N1, N2, N3, N4);
     Enregistrer(Table, IP, IP, +"eth0", 1000);
     --Afficher_Lca_Titre(Table, +"Cache");
     Vider(Table);

     -- Commande: .\routeur -P LFU -c 999 -S -p de_bonbon.txt -t basse.txt -r des_courses.txt
     --Afficher_Parrametres (Nom_Paquet, Nom_Table, Nom_Resultat,
     --                Taille_Cache, Politique, Bavard);


exception
     when Taille_Cache_Exception =>
          Put_Line("La taille du cache doit Ãªtre <1");
     when Not_Txt_Exception =>
          Put_Line("Les noms de fichiers doivent finir par .txt");
     when Not_Politique_Exception =>
          Put_Line("La politique n'est pas valide, les politique acceptÃ©es sont FIFO, LRU et LFU");

end Routeur;

