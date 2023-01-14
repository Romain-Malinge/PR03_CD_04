with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Routeur_Functions;     use Routeur_Functions;
with LCA_IP;                use LCA_IP;


procedure Test_Functions is

    UN_OCTET : constant T_Adresse_IP := 2 ** 8;

    -- Faciliter la transformation String ->  Unbouded_String
    function "+" (Item : in String) return Unbounded_String
                   renames To_Unbounded_String;


    ------------------------- Definition des Tests -----------------------------

    procedure Tester_Set_IP is
        IP : T_Adresse_IP := 0;
    begin
        -- Teste pour une IP quelconque
        Set_IP(IP, 56, 132, 251, 189);
        pragma assert (Natural ((IP / UN_OCTET ** 3) mod UN_OCTET) = 56);
        pragma assert (Natural ((IP / UN_OCTET ** 2) mod UN_OCTET) = 132);
        pragma assert (Natural ((IP / UN_OCTET ** 1) mod UN_OCTET) = 251);
        pragma assert (Natural (IP mod UN_OCTET) = 189);
        -- Teste pour un Masque quelconque
        Set_IP(IP, 255, 255, 255, 0);
        pragma assert (Natural ((IP / UN_OCTET ** 3) mod UN_OCTET) = 255);
        pragma assert (Natural ((IP / UN_OCTET ** 2) mod UN_OCTET) = 255);
        pragma assert (Natural ((IP / UN_OCTET ** 1) mod UN_OCTET) = 255);
        pragma assert (Natural (IP mod UN_OCTET) = 0);
    end Tester_Set_IP;


    procedure Tester_txt_present is
        Nom_Ficher : Unbounded_String;
    begin
        Nom_Ficher := +"table.txt";
        pragma assert (txt_present(Nom_Ficher));
        Nom_Ficher := +"paquet";
        pragma assert (not txt_present(Nom_Ficher));
        Nom_Ficher := +".txt";
        pragma assert (txt_present(Nom_Ficher));
        Nom_Ficher := +"txt";
        pragma assert (not txt_present(Nom_Ficher));
    end Tester_txt_present;


    procedure Tester_To_Adresse_IP is
        IP1 : T_Adresse_IP := 0;
        IP2 : T_Adresse_IP := 0;
        Texte : Unbounded_String;
    begin
        -- Test 1
        Set_IP(IP1, 56, 132, 251, 189);
        Texte := +"56.132.251.189";
        To_Adresse_IP(Texte, IP2);
        pragma assert (IP1 = IP2);
        -- Test 2
        Set_IP(IP1, 255, 255, 255, 0);
        Texte := +"255.255.255.0";
        To_Adresse_IP(Texte, IP2);
        pragma assert (IP1 = IP2);
    end Tester_To_Adresse_IP;


begin

    New_Line;
    Tester_Set_IP;
    Tester_txt_present;
    Tester_To_Adresse_IP;
    Put_Line("Fin des tests : OK");
    New_Line;

end Test_Functions;
