with Prefix_Tree;           use Prefix_Tree;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Routeur_Functions;     use Routeur_Functions;
with LCA_IP;                use LCA_IP;

procedure Test_Prefix_Tree is

     -- Faciliter la transformation String ->  Unbouded_String
     function "+" (Item : in String) return Unbounded_String
                   renames To_Unbounded_String;

     -- D�finition des variables
     Arbre : T_Arbre;         -- Arbre sur lequel on effectuera les tests
     IP : T_Adresse_IP;       -- Variable qui va contenir les IP que l'on va ajouter � Arbre
     Masque : T_Adresse_IP;   -- Variable qui va contenir les masques que l'on va ajouter � Arbre
     Port : Unbounded_String; -- Variable qui va contenir les interfaces que l'on va ajouter � Arbre
     Avancement : Integer;    -- Variable enti�re utile pour la r�cursivit� de la proc�dure Enregistrer du module Prefix_Tree
     Chrono : Integer;        -- Compteur d'ajout � l'arbre
     Min : Integer;           -- Variable utile � la supression de la feuille de plus petit rang

begin

     -- Initialisation des variables
     Initialiser(Arbre);
     Chrono := 0;

     -- Enregistrer des �l�ments dans l'arbre

     -- 1�re feuille
     Set_IP(IP, 192, 0 , 0, 0);
     Set_IP(Masque, 255, 0, 0, 0);
     Port := +"eth1";
     Avancement := 0;
     -- Selon la logique de conception, l'arbre �tant vide, la feuille est cr��e juste � droite de la racine
     Enregistrer(Arbre, IP, Masque, Port, Chrono, 0 , Avancement);
     Chrono := Chrono + 1 ;

     -- 2�me feuille
     Set_IP(IP, 128, 0, 0, 0);
     Set_IP(Masque, 255, 0, 0, 0);
     Port := +"eth2";
     Avancement := 0;
     -- Selon la logique de conception, cela devrait pousser la feuille pr�c�dente pour pouvoir discriminer la nouvelle feuille
     Enregistrer(Arbre, IP, Masque, Port, Chrono, 0 , Avancement);
     Chrono := Chrono + 1;

     -- 3�me feuille
     Set_IP(IP, 96, 0, 0, 0);
     Set_IP(Masque, 255, 0, 0, 0);
     Port := +"eth3";
     Avancement := 0;
     -- Selon la logique de conception, cela ne devrait pousser aucune feuille et cr�er une feuille � gauche de la racine
     Enregistrer(Arbre, IP, Masque, Port, Chrono, 0 , Avancement);
     Chrono := Chrono + 1;
     -- Comme cette feuille existe d�j�, on rafraichit son rang.
     Enregistrer(Arbre, IP, Masque, Port, Chrono, 0 , Avancement);
     -- Chrono := Chrono + 1;

     Afficher_Arbre (Arbre);

     -- Supprimer la feuille avec le rang le plus petit
     Min := Chrono;
     Least_ranked (Arbre, IP, Min);
     Put_Line("La feuille de plus petit rang vaut" & Min'Image);
     Supprimer_Destination (Arbre, IP);

     Afficher_Arbre (Arbre);

     -- Fin du test
     Vider(Arbre);
     if Est_Vide (Arbre) then
          Put_Line (" L'arbre est soigneusement supprim� ");
     else
          Put_Line (" Il faut revoir la fonction Vider.. ");
     end if;

end Test_Prefix_Tree;
