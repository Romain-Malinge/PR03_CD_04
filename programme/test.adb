with Ada.Strings;               use Ada.Strings;	-- pour Both utilisÃ© par Trim
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



-- Commande pour tester les trois politiques:
-- .\routeur_ll -S -c 3 -P <POLITIQUE> -t table_p.txt -p paquet_p.txt

procedure Test (Arbre : in out T_Arbre) is
    IP : T_Adresse_IP;
    Avencement : Integer;
begin
    Initialiser(Arbre);

    Set_IP(IP,0,0,0,0);
    Avencement := 0;
    Enregistrer(Arbre, IP, IP, To_Unbounded_String("eth0"), 0, 0, Avencement);

    Set_IP(IP,255,255,255,255);
    Avencement := 0;
    Enregistrer(Arbre, IP, IP, To_Unbounded_String("eth1"), 1, 1, Avencement);

    Afficher_Arbre(Arbre);

end Test;
