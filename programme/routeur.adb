with Ada.Strings;               use Ada.Strings;	-- pour Both utilisé par Trim
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

    -- Afficher une Lca.
    procedure Afficher_Lca is
            new Pour_Chaque (Afficher_Cellule);

    -- Afficher une Lca avec des titres.
    procedure Afficher_Lca_Titre (Lca : in T_LCA_IP; Titre : Unbounded_String) is
    begin
        New_Line;
        Put_Line ("--------------------------- " & Titre & " ----------------------------");
        Put_line ("Destination         Masque              Interface    Fréquence");
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

    Table : T_LCA_IP;
    IP : LCA_IP.T_Adresse_IP;
    N1, N2, N3, N4 : LCA_IP.T_Adresse_IP;
    Paquet : File_Type;
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
        -- Traiter le a éme argument de la ligne de commande
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

    -- Teste temporaire de Lca
    Initialiser (Table);
    N1 := 110; N2 := 120; N3 := 130; N4 := 140;
    Set_IP(IP, N1, N2, N3, N4);
    Enregistrer(Table, IP, IP, +"eth1", 1000);
    N1 := 150; N2 := 160; N3 := 170; N4 := 180;
    Set_IP(IP, N1, N2, N3, N4);
    Enregistrer(Table, IP, IP, +"eth0", 1000);
    Afficher_Lca_Titre(Table, +"Cache");
    Vider(Table);

    -- Commande: .\routeur -P LFU -c 999 -S -p de_bonbon.txt -t basse.txt -r des_courses.txt
    Afficher_Parrametres (Nom_Paquet, Nom_Table, Nom_Resultat,
                          Taille_Cache, Politique, Bavard);


exception
    when Taille_Cache_Exception =>
        Put_Line("La taille du cache doit être <1");
    when Not_Txt_Exception =>
        Put_Line("Les noms de fichiers doivent finir par .txt");
    when Not_Politique_Exception =>
        Put_Line("La politique n'est pas valide, les politique acceptées sont FIFO, LRU et LFU");
    when others =>
        null;

end Routeur;

