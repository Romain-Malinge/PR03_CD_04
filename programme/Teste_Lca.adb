with Ada.Strings;               use Ada.Strings;	-- pour Both utilisé par Trim
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Exceptions;            use Ada.Exceptions;	-- pour Exception_Message
with LCA_IP;                    use LCA_IP;
with Routeur_Exceptions;        use Routeur_Exceptions;

procedure Teste_Lca is

    UN_OCTET : constant T_Adresse_IP := 2 ** 8;

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
    Put (Natural ((IP / UN_OCTET ** 3) mod UN_OCTET), 1); Put (".");
    Put (Natural ((IP / UN_OCTET ** 2) mod UN_OCTET), 1); Put (".");
    Put (Natural ((IP / UN_OCTET ** 1) mod UN_OCTET), 1); Put (".");
    Put (Natural  (IP mod UN_OCTET), 1);
    end Afficher_IP;

    -- Afficher une cellule de Lca.
    procedure Afficher (D : in T_Adresse_IP;
                        M : in T_Adresse_IP;
                        P : in Unbounded_String;
                        F : in Integer) is
    begin
        Afficher_IP(D); Put("    ");
        Afficher_IP(D); Put("    ");
        Put(P); Put("        ");
        Put_Line(F'Image);
    end Afficher;

    -- Afficher une Lca.
    procedure Afficher_Lca is
            new Pour_Chaque (Afficher);

    -- Transformer un String en un Unbounded_String
    function "+" (Item : in String) return Unbounded_String
                   renames To_Unbounded_String;

    -- Transformer un Unbounded_String en un String
    function "-" (Item : in Unbounded_String) return String
                   renames To_String;

    -- Variables
    Table : T_LCA_IP;
    IP_Teste : T_Adresse_IP;

begin

    Initialiser (Table);
    Set_IP(IP_Teste, 110, 120, 130, 140);
    Enregistrer(Table, IP_Teste, IP_Teste, +"port1", 1000);
    Set_IP(IP_Teste, 150, 160, 170, 180);
    Enregistrer(Table, IP_Teste, IP_Teste, +"port2", 1000);
    New_Line;
    Put_line ("Destination        Masque             Interface     Fréquence");
    Afficher_Lca(Table);
    Vider(Table);
    if Est_Vide(Table) then
        Put_Line ("La table est bien vide !");
    end if;
    New_Line;

end Teste_Lca;
