with Ada.Strings;               use Ada.Strings;	-- pour Both utilisé par Trim
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Exceptions;            use Ada.Exceptions;	-- pour Exception_Message
with LCA_IP;                    use LCA_IP;
with Routeur_Exceptions;        use Routeur_Exceptions;



package body Routeur_Functions is


    UN_OCTET : constant T_Adresse_IP := 2 ** 8;


    procedure Set_IP (IP : in out LCA_IP.T_Adresse_IP;
                      N1 : in out LCA_IP.T_Adresse_IP;
                      N2 : in out LCA_IP.T_Adresse_IP;
                      N3 : in out LCA_IP.T_Adresse_IP;
                      N4 : in out LCA_IP.T_Adresse_IP) is
    begin

        N1 := 00000111;
        IP := N1;
        IP := IP * UN_OCTET + N2;
        IP := IP * UN_OCTET + N3;
        IP := IP * UN_OCTET + N4;
    end Set_IP;


    procedure Afficher_IP (IP : in T_Adresse_IP) is
    begin
        Put (Natural ((IP / UN_OCTET ** 3) mod UN_OCTET), 3); Put (".");
        Put (Natural ((IP / UN_OCTET ** 2) mod UN_OCTET), 3); Put (".");
        Put (Natural ((IP / UN_OCTET ** 1) mod UN_OCTET), 3); Put (".");
        Put (Natural  (IP mod UN_OCTET), 3);
    end Afficher_IP;



    procedure Afficher_Cellule (D : in LCA_IP.T_Adresse_IP;
                                M : in LCA_IP.T_Adresse_IP;
                                P : in Unbounded_String;
                                F : in Integer) is
    begin
        Afficher_IP(D); Put("     ");
        Afficher_IP(D); Put("     ");
        Put(P); Put("        ");
        Put_Line(F'Image);
    end Afficher_Cellule;


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


end Routeur_Functions;
