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


---------------   DEMONSTRATIONS   ---------------

-- Commande pour vérifier les tailles des masques du cache:
-- .\routeur_la -p paquet_tailles_masques.txt -t table_tailles_masques.txt -S

-- Commande pour tester les trois politiques:
-- .\routeur_la -p paquet_teste_politique.txt -t table_teste_politique.txt -c 3 -S -P XXX


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
