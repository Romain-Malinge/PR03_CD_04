with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;         use Ada.Float_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Exceptions;            use Ada.Exceptions;
with LCA_IP;                    use LCA_IP;
with Routeur_Exceptions;        use Routeur_Exceptions;


package body Routeur_Functions is

    UN_OCTET : constant T_Adresse_IP := 2 ** 8;


    procedure Set_IP (IP : in out LCA_IP.T_Adresse_IP;
                      N1 : in LCA_IP.T_Adresse_IP;
                      N2 : in LCA_IP.T_Adresse_IP;
                      N3 : in LCA_IP.T_Adresse_IP;
                      N4 : in LCA_IP.T_Adresse_IP) is
    begin
        IP := N1;
        IP := IP * UN_OCTET + N2;
        IP := IP * UN_OCTET + N3;
        IP := IP * UN_OCTET + N4;
    end Set_IP;


    procedure Put_IP (IP : in T_Adresse_IP) is
    begin
        Put (Natural ((IP / UN_OCTET ** 3) mod UN_OCTET), 3); Put (".");
        Put (Natural ((IP / UN_OCTET ** 2) mod UN_OCTET), 3); Put (".");
        Put (Natural ((IP / UN_OCTET ** 1) mod UN_OCTET), 3); Put (".");
        Put (Natural  (IP mod UN_OCTET), 3);
    end Put_IP;


    procedure Afficher_Cellule (D : in out LCA_IP.T_Adresse_IP;
                                M : in out LCA_IP.T_Adresse_IP;
                                P : in out Unbounded_String;
                                F : in out Integer) is
    begin
        Put("| ");
        Put_IP(D); Put("     ");
        Put_IP(M); Put("     ");
        Put(F,3); Put("          ");
        Put_Line(P);
    end Afficher_Cellule;


    procedure Afficher_Parametres (Nom_Paquet : Unbounded_String;
                                   Nom_Table : Unbounded_String;
                                   Nom_Resultat : Unbounded_String;
                                   Taille_Cache : Integer;
                                   Politique : Unbounded_String;
                                   Nbr_Route : Integer;
                                   Nbr_Ajoute : Integer;
                                   Num_Ligne : Ada.Text_IO.Count) is
        Defaut_Cache : Float;
    begin
        New_Line;
        Put_Line ("+-------------------- STATISTIQUES ligne" & Num_Ligne'Image & " ---------------------");
        -- Ligne 1 des statistiques
        Put ("| Politique du Cache:   ");
        Put (Politique);
        if not (Politique = "FIFO") then
            Put (" ");
        else
            null;
        end if;
        Put ("    | Nbr d'IP routé: "); Put (Nbr_Route, 3); New_Line;
        -- Ligne 2 des statistiques
        Put ("| Taille du Cache:"); Put (Taille_Cache, 4);
        Put ("          | Paquet:   ");
        Put_Line(Nom_Paquet);
        -- Ligne 3 des statistiques
        Put ("| Ajout au Cache: "); Put (Nbr_Ajoute, 4);
        Put ("          | Table:    "); Put_Line(Nom_Table);
        -- Ligne 4 des statistiques
        Defaut_Cache := Float(Nbr_Ajoute)/Float(Nbr_Route) * 100.0;
        Put ("| Defaut de cache: "); Put(Defaut_Cache, 3,0,0); Put(" %");
        Put ("      | Résultat: "); Put_Line(Nom_Resultat);
        Put_Line ("+---------------------------------------------------------------");
    end Afficher_Parametres;


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


    procedure Get_IP (Fichier : in out File_type; IP : in out T_Adresse_IP) is
        Entier : Integer;
        Caractere : Character;
    begin
        Get(Fichier, Entier);
        IP := T_Adresse_IP(Entier);
        for i in 1..3 loop
            Get(Fichier, Caractere);
            Get(Fichier, Entier);
            IP :=  IP * UN_OCTET + T_Adresse_IP(Entier);
        end loop;
    end Get_IP;


    procedure To_Adresse_IP (Texte : Unbounded_String; IP : in out T_Adresse_IP) is
        Nombre : Integer;
        c : Character;
    begin
        Nombre := 0;
        for i in 1..Length(Texte) loop
            c := To_String(Texte)(i);
            if c in '0'..'9' then
                begin
                    Nombre := Nombre * 10 + (Character'Pos (c) - 16#30#);
                exception
                    when CONSTRAINT_ERROR =>
                        raise Paquet_Invalide_Exception;
                end;
            elsif c = '.' then
                IP := IP * UN_OCTET + T_Adresse_IP(Nombre);
                Nombre := 0;
            end if;
        end loop;
        IP := IP * UN_OCTET + T_Adresse_IP(Nombre);
    end To_Adresse_IP;


    procedure Put_IP_Interface (Fichier : File_Type;
                                IP : T_Adresse_IP;
                                Port : Unbounded_String) is
    begin
        Put (Fichier, Natural ((IP / UN_OCTET ** 3) mod UN_OCTET), 1);
        Put (Fichier, ".");
        Put (Fichier, Natural ((IP / UN_OCTET ** 2) mod UN_OCTET), 1);
        Put (Fichier, ".");
        Put (Fichier, Natural ((IP / UN_OCTET ** 1) mod UN_OCTET), 1);
        Put (Fichier, ".");
        Put (Fichier, Natural  (IP mod UN_OCTET), 1);
        Put (Fichier, " ");
        Put (Fichier, Port);
        New_Line (Fichier);
    end Put_IP_Interface;


end Routeur_Functions;

