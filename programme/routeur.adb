with Ada.Strings;               use Ada.Strings;	-- pour Both utilisé par Trim
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Exceptions;            use Ada.Exceptions;	-- pour Exception_Message
with LCA_IP;                    use LCA_IP;
with Routeur_Exceptions;        use Routeur_Exceptions;


procedure Routeur is

    ------------------------- Définition des constant --------------------------

    UN_OCTET : constant T_Adresse_IP := 2 ** 8;


    --------------------- Espace pour les sous-programmes ----------------------

    -- Initialiser une adresse IP
    procedure Set_IP (IP : in out T_Adresse_IP;
                      N1 : T_Adresse_IP;
                      N2 : T_Adresse_IP;
                      N3 : T_Adresse_IP;
                      N4 : T_Adresse_IP) is
    begin
        IP := N1;
        IP := IP * UN_OCTET + N2;
        IP := IP * UN_OCTET + N3;
        IP := IP * UN_OCTET + N4;
    end Set_IP;

    -- Afficher une adresse IP
    procedure Afficher_IP (IP : in T_Adresse_IP) is
    begin
    Put (Natural ((IP / UN_OCTET ** 3) mod UN_OCTET), 3); Put (".");
    Put (Natural ((IP / UN_OCTET ** 2) mod UN_OCTET), 3); Put (".");
    Put (Natural ((IP / UN_OCTET ** 1) mod UN_OCTET), 3); Put (".");
    Put (Natural  (IP mod UN_OCTET), 3);
    end Afficher_IP;

    -- Afficher une cellule de Lca
    procedure Afficher (D : in T_Adresse_IP;
                        M : in T_Adresse_IP;
                        P : in Unbounded_String;
                        F : in Integer) is
    begin
        Afficher_IP(D); Put("     ");
        Afficher_IP(D); Put("     ");
        Put(P); Put("        ");
        Put_Line(F'Image);
    end Afficher;

    -- Afficher une Lca.
    procedure Afficher_Lca is
            new Pour_Chaque (Afficher);


    -- Afficher une Lca avec des titres.
    procedure Afficher_Lca_Titre (Table : in T_LCA_IP) is
    begin
        New_Line;
        Put_line ("Destination         Masque              Interface    Fréquence");
        Afficher_Lca (Table);
        New_Line;
    end Afficher_Lca_Titre;

    -- Afficher les paramettres du programmes
    procedure Afficher_Parrametres (Nom_Paquet : Unbounded_String;
                                    Nom_Table : Unbounded_String;
                                    Nom_Resultat : Unbounded_String;
                                    Taille_Cache : Integer;
                                    Politique : Unbounded_String;
                                    Bavard : Boolean) is
    begin
        New_Line;
        Put_Line ("--------------------- PARRAMETRES ---------------------");
        Put ("Politique du Cache: ");
        Put (Politique);
        if not (Politique = "FIFO") then
            Put (" ");
        else
            null;
        end if;
        Put ("   |  Paquet: ");
        Put_Line(Nom_Paquet);
        Put ("Taille du Cache:"); Put (Taille_Cache, 4);
        Put ("       |  Table: "); Put_Line(Nom_Table);
        Put ("Bavard: ");
        if Bavard then
            Put ("oui");
        else
            Put ("non");
        end if;
        Put ("                |  Résultat: "); Put_Line(Nom_Resultat);
        New_Line;
    end Afficher_Parrametres;


    -- Transformer un String en un Unbounded_String
    function "+" (Item : in String) return Unbounded_String
                   renames To_Unbounded_String;

    -- Transformer un Unbounded_String en un String
    function "-" (Item : in Unbounded_String) return String
                   renames To_String;

    -- Vérifier que le motif .txt est dans un Unbounded_String
    function txt_present (Mot : in Unbounded_String) return Boolean is
        Taille : Integer;
    begin
        Taille := Length(Mot);
        if Taille < 4 then
            return False;
        else
            null;
            return (To_String(Mot)(Taille) = 't' and
                    To_String(Mot)(Taille-1) = 'x' and
                    To_String(Mot)(Taille-2) = 't' and
                    To_String(Mot)(Taille-3) = '.');
        end if;
    end txt_present;


    --------------------- Variables globales du programme ----------------------

    Table : T_LCA_IP;
    IP_Teste : T_Adresse_IP;
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
    Set_IP(IP_Teste, 110, 120, 130, 140);
    Enregistrer(Table, IP_Teste, IP_Teste, +"eth1", 1000);
    Set_IP(IP_Teste, 150, 160, 170, 180);
    Enregistrer(Table, IP_Teste, IP_Teste, +"eth0", 1000);
    -- Afficher_Lca_Titre(Table);
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

