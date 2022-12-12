with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with LCA_IP;                    use LCA_IP;


package Routeur_Functions is

    -- Initialiser une adresse IP
    procedure Set_IP (IP : in out LCA_IP.T_Adresse_IP;
                      N1 : in out LCA_IP.T_Adresse_IP;
                      N2 : in out LCA_IP.T_Adresse_IP;
                      N3 : in out LCA_IP.T_Adresse_IP;
                      N4 : in out LCA_IP.T_Adresse_IP);


    -- Afficher une adresse IP
    procedure Afficher_IP (IP : in LCA_IP.T_Adresse_IP);


    -- Afficher les paramettres du programmes
    procedure Afficher_Parrametres (Nom_Paquet : Unbounded_String;
                                    Nom_Table : Unbounded_String;
                                    Nom_Resultat : Unbounded_String;
                                    Taille_Cache : Integer;
                                    Politique : Unbounded_String;
                                    Bavard : Boolean);


    -- Afficher une cellule de Lca
    procedure Afficher_Cellule (D : in LCA_IP.T_Adresse_IP;
                                M : in LCA_IP.T_Adresse_IP;
                                P : in Unbounded_String;
                                F : in Integer);


    -- Vérifier que le motif .txt est dans un Unbounded_String
    function txt_present (Mot : in Unbounded_String) return Boolean;


end Routeur_Functions;
