with Ada.Strings;                use Ada.Strings;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Integer_Text_IO;        use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;   use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.IO_Exceptions;          use Ada.IO_Exceptions;
with LCA_IP;                     use LCA_IP;
with Prefix_Tree;                use Prefix_Tree;
with Routeur_Exceptions;         use Routeur_Exceptions;
with Routeur_Functions;          use Routeur_Functions;


procedure routeur_la is
    
    --------------------------------- VARIABLES --------------------------------
    
    UN_OCTET : constant T_Adresse_IP := 2 ** 8;
    
    Nom_Table : Unbounded_String;     -- Le nom du fichier contenant la table
    Nom_Paquet : Unbounded_String;    -- Le nom du fichier contenant les paquets
    Nom_Resultat : Unbounded_String;  -- Le nom du fichier contenant le résultat
    F_Table : File_Type;              -- Le ficher Table
    F_Paquet : File_Type;             -- Le ficher Paquet
    F_Resultat : File_Type;           -- Le ficher Resultat
    Num_Ligne : Ada.Text_IO.Count;    -- le numéro de la ligne courante
    Table : T_LCA_IP;                 -- La Table de routage sous forme de Lca
    Cache : T_Arbre;                  -- Le Cache sous forme d'Arbre
    IP : T_Adresse_IP;                -- L'addresse IP a router
    Destination : T_Adresse_IP;       -- La destination de l'IP
    A_Sprimmer : T_Adresse_IP;        -- La destination a supprimer du cache
    Masque : T_Adresse_IP;            -- Le masque de la destination
    Grand_Masque : T_Adresse_IP;      -- Le masque qui permet d'eviter les erreurs de cache
    Port : Unbounded_String;          -- L'interface de l'adresse IP
    Frequence : Integer;              -- La fréquence d'utilisation dans le cache
    Rang : Integer;                   -- Le rang dans le cache
    Avancement : Integer;             -- L'avencement de la recherche dans le cache
    Min : Integer;                    -- Le rang minimum dans le cache
    Ligne : Unbounded_String;         -- Une ligne de texte
    Taille_Cache : Integer;           -- La taille maximale du cache
    Politique : Unbounded_String;     -- La politique du cache
    Fin : Boolean;                    -- La variable qui indique la fin du programme
    Bavard : Boolean;                 -- Indique si l'utilisateur demande les statistiques
    Nbr_Route : Integer;              -- Le nombre d'IP routé
    Nbr_Ajoute : Integer;             -- Le nombre de route ajouté au cache
    
    
    ------------------------------- PROCEDURES ---------------------------------
    
    -- Transformer un String en un Unbounded_String
    function "+" (Item : in String) return Unbounded_String
                  renames To_Unbounded_String;

    -- Transformer un Unbounded_String en un String
    function "-" (Item : in Unbounded_String) return String
                  renames To_String;

    -- Afficher une Lca.
    procedure Afficher_Lca is new Pour_Chaque (Afficher_Cellule);

    -- Afficher une Lca avec un titres.
    procedure Afficher_Lca_Titre (Lca : in T_LCA_IP; Titre : Unbounded_String; Num_Ligne : Ada.Text_IO.Count) is
    begin
        New_Line;
        Put_Line ("+----------------------- " & Titre & " ligne" & Num_Ligne'Image & " -------------------------");
        Put_line ("| Destination         Masque              Interface    Fréquence");
        Put_Line("|");
        Afficher_Lca (Lca);
        Put_Line ("+---------------------------------------------------------------");
    end Afficher_Lca_Titre;
    
    -- Compare une IP à un couples Destination et Masque
    procedure Comparer_Cellule (D : in out LCA_IP.T_Adresse_IP;
                                M : in out LCA_IP.T_Adresse_IP;
                                P : in out Unbounded_String;
                                F : in out Integer) is
    begin
        if ((IP and M) = D) and M >= Masque then
            Destination := D;
            Masque := M;
            Port := P;
            Frequence := F;
        end if;
    end Comparer_Cellule;
    
    -- Compare une IP à tout les couples Destination et Masque d'une Lca
    procedure Comparer_Lca is new Pour_Chaque (Comparer_Cellule);

    
------------------------------ DEBUT DU PROGRAMME ------------------------------ 
begin

    -- Initialisation des variables
    Nom_Paquet := +"paquet.txt";
    Nom_Table := +"table.txt";
    Nom_Resultat := +"resultats.txt";
    Taille_Cache := 10;
    Politique := +"FIFO";
    Bavard := True;
    Fin := False;
    A_Sprimmer := 0;
    Grand_Masque := 0;
    Nbr_Route := 0;
    Nbr_Ajoute := 0;
    
    
    -- Traiter les arguments de la ligne de commande
    for arg in 1..Argument_Count loop
        -- Traiter le a ieme argument de la ligne de commande
        if Argument(arg)(1) = '-' then
            case Argument(arg)(2) is
            when 'c' =>
                begin
                    Taille_Cache := Integer'Value (Argument(arg+1));
                exception
                    when CONSTRAINT_ERROR => raise Cache_Exception;
                end;
            when 'P' => Politique := +Argument(arg+1);
            when 'S' => Bavard := False;
            when 'p' => Nom_Paquet := +Argument(arg+1);
            when 't' => Nom_Table := +Argument(arg+1);
            when 'r' => Nom_Resultat := +Argument(arg+1);
            when others => null;
            end case;
        else
            null;
        end if;
    end loop;
    
    
    -- Lever les erreurs du à la ligne de commande
    if Taille_Cache < 1 then
        raise Cache_Exception;
    elsif not txt_present(Nom_Paquet) then
        raise Not_Txt_Exception;
    elsif not txt_present(Nom_Table) then
        raise Not_Txt_Exception;
    elsif not txt_present(Nom_Resultat) then
        raise Not_Txt_Exception;
    elsif (Politique /= "FIFO" and Politique /= "LRU" and Politique/= "LFU") then
        raise Not_Politique_Exception;
    end if;
    
    
    -- Ouvrir les fichiers
    begin
        Open (F_Table , In_File, -Nom_Table);
    exception
        when ADA.IO_EXCEPTIONS.NAME_ERROR =>
            raise Table_Not_Found_Exception;    -- si le fichier table manque
    end;
    begin
        Open (F_Paquet, In_File, -Nom_Paquet);
    exception
        when ADA.IO_EXCEPTIONS.NAME_ERROR =>
            raise Paquet_Not_Found_Exception;   -- si le fichier paquet manque
    end;
    Create (F_Resultat, Out_File, -Nom_Resultat);
    
    
    -- Passer la Table sous forme de Lca
    Initialiser(Table);
    while not End_Of_File (F_Table) loop
        -- Enregistrer une ligne du fichier Lca
        Num_Ligne := Line(F_Table);
        if End_Of_Line (F_Table) then
            Skip_Line(F_Table);
        else
            Destination := 0;
            begin
                Get_IP(F_Table, Destination);
                Get_IP(F_Table, Masque);
                Get_Line (F_Table, Ligne);
            exception
                when ADA.IO_EXCEPTIONS.DATA_ERROR =>
                    raise Table_Invalide_Exception;
            end;
            Trim (Ligne, Both);
            Enregistrer (Table, Destination, Masque, Ligne, 0);
        end if;
    end loop;
    
   
    -- Traiter le fichier Paquet
    while (not End_Of_File (F_Paquet)) and not Fin loop
        Num_Ligne := Line(F_Paquet);
        Get_Line (F_Paquet, Ligne);
        Trim (Ligne, Both);
        
        -- Les cas de non Adresse IP
        if Length(Ligne)=0 then
            null;
        elsif Ligne = +"stat" then
            Afficher_Parametres (Nom_Paquet, Nom_Table, Nom_Resultat, Taille_Cache, Politique, Nbr_Route, Nbr_Ajoute, Num_Ligne);
        elsif Ligne = +"table" then
            Afficher_Lca_Titre (Table, +"TABLE", Num_Ligne);
        elsif Ligne = +"cache" then
            New_Line;
            Put_Line ("+----------------------- CACHE ligne" & Num_Ligne'Image & " -------------------------");
            Put_line ("| Destination         Masque              Interface    Fréquence");
            Put_Line("|");
            Afficher_Arbre (Cache);
            Put_Line ("+---------------------------------------------------------------");
        elsif Ligne = +"fin" then
            Fin := True;
            New_Line;
            Put_Line ("FIN ligne" & Num_Ligne'Image);
            
        -- Router une Adresse IP
        elsif To_String(Ligne)(1) in '0'..'9' then
            Nbr_Route := Nbr_Route+ 1;
            IP := 0;
            Masque := 0;
            Port := +"";
            To_Adresse_IP (Ligne, IP);
            Comparer_Arbre (Cache, IP, Destination, Masque, Port, Frequence);  -- Actualise Port des mas rang freq
            
            -- Le cache ne peut pas router l'IP
            if Port = "" then
                Nbr_Ajoute := Nbr_Ajoute + 1;
                Comparer_Lca (Table);
                Grand_Masque := Masque;
                Trouver_Grand_Masque(Table, IP, Destination, Grand_Masque);
                
                -- Enregistrer la nouvelle feuille
                Rang := Integer(Num_Ligne);
                Frequence := 0;
                Avancement := 0;
                Enregistrer (Cache, (IP and Grand_Masque), Grand_Masque, Port, Rang, Frequence, Avancement);
                
                -- Supprimer le plus bas Rang si la taille est trop grande
                if Taille(Cache) > Taille_Cache then
                    Min := Integer(Num_Ligne);
                    Least_ranked (Cache, A_Sprimmer, Min);
                    Supprimer_Destination (Cache, A_Sprimmer);
                else
                    null;
                end if;
                
                -- Modifier le rang de la nouvelle feuille avec 0 (sinon elle aurait était supprimée)
                if Politique = +"LFU" then
                    Avancement := 0;
                    Enregistrer (Cache, (IP and Grand_Masque), Grand_Masque, Port, Frequence, Frequence, Avancement);
                else
                    null;
                end if;
                
            -- Le cache a routé l'IP
            else
                Frequence := Frequence + 1;
                if Politique = +"LRU" then
                    Rang := Integer(Num_Ligne);
                elsif Politique = +"LFU" then
                    Rang := Frequence;
                else
                    null;
                end if;
                Avancement := 0;
                Grand_Masque := Masque;
                Trouver_Grand_Masque(Table, IP, Destination, Grand_Masque);
                Enregistrer (Cache, (IP and Grand_Masque), Grand_Masque, Port, Rang, Frequence, Avancement);
            end if;
            
            Put_IP_Interface (F_Resultat, IP, Port);  -- Ajouter une ligne dans le fichier resultat
        
        -- La lignes ne correspond à rien
        else
            New_Line;
            Put("La ligne n°" & Num_Ligne'Image);
            Put_Line(" du fichier " & Nom_Paquet & " est incorecte (elle n'a pas été prise en compte)");
        end if;
    end loop;
    
    
    -- Les instructions de fin de programme
    Vider (Table);
    Vider (Cache);
    Close (F_Table);
    Close (F_Paquet);
    Close (F_Resultat);
    if Bavard then
        Afficher_Parametres (Nom_Paquet, Nom_Table, Nom_Resultat, Taille_Cache, Politique, Nbr_Route, Nbr_Ajoute, Num_Ligne);
    else
        null;
    end if;
    New_Line;
    

---------------------------------- EXEPTIONS -----------------------------------
exception
        
    when Cache_Exception =>
        Put_Line("/!\ ERREUR /!\ La taille du cache doit être un entier >=1");
    when Not_Txt_Exception =>
        Put_Line("/!\ ERREUR /!\ Le nom des fichiers doivent finir par .txt");
    when Not_Politique_Exception =>
        Put_Line("/!\ ERREUR /!\ La politique " & Politique & " n'est pas valide, les politique acceptÃ©es sont FIFO, LRU et LFU");
    when Table_Not_Found_Exception =>
        Put_Line("/!\ ERREUR /!\ Le fichier " & Nom_Table & " n'est pas présent dans le répertoire");
    when Paquet_Not_Found_Exception =>
        Put_Line("/!\ ERREUR /!\ Le fichier " & Nom_Paquet & " n'est pas présent dans le répertoire");
    when Table_Invalide_Exception =>
        Put_Line("/!\ ERREUR /!\ La ligne" & Num_Ligne'Image & " du fichier " & Nom_Table & " est incorrecte");
    when Paquet_Invalide_Exception =>
        Put_Line("/!\ ERREUR /!\ La ligne" & Num_Ligne'Image & " du fichier " & Nom_Paquet & " est incorrecte");
        
end routeur_la;
