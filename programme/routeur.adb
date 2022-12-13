with Ada.Strings;               use Ada.Strings;	-- pour Both utilisÃ© par Trim
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Exceptions;            use Ada.Exceptions;	-- pour Exception_Message
with Ada.IO_Exceptions;         use Ada.IO_Exceptions;
with LCA_IP;                    use LCA_IP;
with Routeur_Exceptions;        use Routeur_Exceptions;
with Routeur_Functions;         use Routeur_Functions;


procedure Routeur is
    
    
    --------------------- Variables globales du programme ----------------------
    
    UN_OCTET : constant T_Adresse_IP := 2 ** 8;
    
    Nom_Table : Unbounded_String;     -- Le nom du fichier contenant la table
    Nom_Paquet : Unbounded_String;    -- Le nom du fichier contenant les paquets
    Nom_Resultat : Unbounded_String;  -- Le nom du fichier contenant le r\u00e9sultat
    F_Table : File_Type;              -- Le ficher Table
    F_Paquet : File_Type;             -- Le ficher Paquet
    F_Resultat : File_Type;           -- Le ficher Resultat
    Table : T_LCA_IP;                 -- La Table de routage sous forme de Lca
    Cache : T_LCA_IP;                 -- Le Cache sous forme de Lca
    IP : T_Adresse_IP;                -- L'addresse IP a router
    Destination : T_Adresse_IP;       -- La destination de l'IP
    Masque : T_Adresse_IP;            -- Le masque de la destination
    Port : Unbounded_String;          -- L'interface de l'adresse IP
    Ligne : Unbounded_String;         -- Une ligne de texte
    Taille_Cache : Integer;           -- La taille maximale du cache
    Politique : Unbounded_String;     -- La politique du cache
    Fin : Boolean;                    -- La variable qui indique la fin du programme
    Bavard : Boolean;                 -- Indique si l'utilisateur demande les statistiques
    Nbr_Ajoute : Integer;             -- Le nombre de route ajout\u00e9es au cache
    
    
    ------------------------------- Procedures ---------------------------------

    -- Afficher une Lca.
    procedure Afficher_Lca is
            new Pour_Chaque (Afficher_Cellule);

    -- Afficher une Lca avec des titres.
    procedure Afficher_Lca_Titre (Lca : in T_LCA_IP; Titre : Unbounded_String) is
    begin
        New_Line;
        Put_Line ("+--------------------------- " & Titre & " -----------------------------");
        Put_line ("| Destination         Masque              Interface    Fréquence");
        Put_Line("| ");
        Afficher_Lca (Lca);
        Put_Line ("+---------------------------------------------------------------");
    end Afficher_Lca_Titre;

    -- Transformer un String en un Unbounded_String
    function "+" (Item : in String) return Unbounded_String
                  renames To_Unbounded_String;

    -- Transformer un Unbounded_String en un String
    function "-" (Item : in Unbounded_String) return String
                  renames To_String;
    
    -- Compare une IP à un couples Destination et Masque
    procedure Comparer_Cellule (D : in out LCA_IP.T_Adresse_IP;
                                M : in out LCA_IP.T_Adresse_IP;
                                P : in out Unbounded_String;
                                F : in out Integer) is
    begin
        if ((IP and M) = D) and M > Masque then
            Port := P;
            Masque := M;
            F := F + 1;
        end if;
    end Comparer_Cellule;
    
    -- Compare une IP à tout les couples Destination et Masque d'une Lca
    procedure Comparer_Lca is
            new Pour_Chaque (Comparer_Cellule);
    
    
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
    -- Commande: .\routeur -P LFU -c 999 -S -p de_bonbon.txt -t basse.txt -r des_courses.txt
    
    
    -- Lever les erreurs du à la ligne de commande
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
    end loop;
    Close (F_Table);
    
    
    -- Rediriger les IP du fichier Paquet
    while (not End_Of_File (F_Paquet)) and not Fin loop
        Get_Line (F_Paquet, Ligne);
        Trim (Ligne, Both);
        if Ligne = +"stat" then
            Afficher_Parrametres (Nom_Paquet, Nom_Table, Nom_Resultat, Taille_Cache, Politique, Nbr_Ajoute);
        elsif Ligne = +"table" then
            Afficher_Lca_Titre (Table, +"TABLE");
        elsif Ligne = +"cache" then
            Afficher_Lca_Titre (Cache, +"CACHE");
        elsif Ligne = +"fin" then
            Fin := True;
        elsif To_String(Ligne)(1) in '0'..'9' then
            -- Router une Adresse IP
            IP := 0;
            To_Adresse_IP (Ligne, IP);
            Masque := 0;
            Comparer_Lca (Table);  -- Modifie la variable Port
            Put_IP_Interface (F_Resultat, IP, Port);
        else
            New_Line;
            Put("La ligne n°"); Put(Line(F_Paquet)'Image);
            Put(" du fichier " & Nom_Paquet);
            Put_Line(" est incorecte (elle n'a pas été prise en compte)");
        end if;
    end loop;
    
    
    -- Les trucs de fin
    Vider (Table);
    Vider (Cache);
    Close (F_Paquet);
    Close (F_Resultat);
    New_Line; 
    
    
exception
    when Taille_Cache_Exception =>
        Put_Line("/!\ ERREUR /!\ La taille du cache doit être <1");
    when Not_Txt_Exception =>
        Put_Line("/!\ ERREUR /!\ Le nom des fichiers doivent finir par .txt");
    when Not_Politique_Exception =>
        Put_Line("/!\ ERREUR /!\ La politique n'est pas valide, les politique acceptÃ©es sont FIFO, LRU et LFU");
    when Table_Not_Found_Exception =>
        Put_Line("/!\ ERREUR /!\ Le fichier " & Nom_Table & " n'est pas présent dans le répertoire");
    when Paquet_Not_Found_Exception =>
        Put_Line("/!\ ERREUR /!\ Le fichier " & Nom_Paquet & " n'est pas présent dans le répertoire");
    when Table_Invalide_Exception =>
        Put_Line("/!\ ERREUR /!\ Le fichier " & Nom_Table & " ne respecte pas les normes d'écriture");
    when Paquet_Invalide_Exception =>
        Put_Line("/!\ ERREUR /!\ Le fichier " & Nom_Paquet & " ne respecte pas les normes d'écriture");
    --when others =>
        --Put ("erreur pas prévue");
    
end Routeur;

