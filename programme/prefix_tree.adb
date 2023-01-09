with Ada.Text_IO;            use Ada.Text_IO;
with Routeur_Exceptions;     use Routeur_Exceptions;
with Ada.Unchecked_Deallocation;
with Routeur_Functions;         use Routeur_Functions;

package body Prefix_Tree is

     procedure Free is new Ada.Unchecked_Deallocation (Object => T_Noeud,
                                                       Name => T_Arbre);


     procedure Initialiser (Arbre : out T_Arbre) is

     begin
          Arbre := Null;
     end Initialiser;


     function Est_Vide (Arbre : T_Arbre) return Boolean is

     begin
          return Arbre = Null;
     end Est_Vide;


     procedure Afficher (Arbre : in T_Arbre) is

     begin
          if Est_Vide(Arbre) then
               null;
          else
               -- On affiche de gauche Ã  droite
               Afficher(Arbre.all.Gauche);

               -- Si on tombe sur une feuille, on affiche ses donnÃ©es
               if Arbre.all.Feuille then
                    Put_IP(Arbre.all.Destination);
                    Put_IP(Arbre.all.Masque);
                    Put_Line( To_String(Arbre.all.Port) );
                    New_Line;
               else
                    null;
               end if;
               Afficher(Arbre.all.Droite);
          end if;
     end Afficher;


     function Taille (Arbre : in T_Arbre) return Integer is

     begin
          if Est_Vide(Arbre) then
               return 0;
          elsif Arbre.all.Feuille then
               return 1;
          else
               return ( Taille(Arbre.all.Gauche) + Taille(Arbre.all.Droite) );
          end if;
     end Taille;

     -- Sous-programme utile pour déterminer le rang maximum dans l'arbre
     function Rang_Max (Arbre : in T_Arbre) return Integer is

     begin
          if Est_Vide(Arbre) then
               return 0;
          elsif Arbre.all.Feuille then
               return Arbre.all.Rang;
          elsif Rang_Max(Arbre.all.Droite) > Rang_Max(Arbre.all.Gauche) then
               return Rang_Max(Arbre.all.Droite);
          else
               return Rang_Max(Arbre.all.Gauche);
          end if;
     end Rang_Max;

     function Comparer_Arbre (Arbre : in T_Arbre ;
                              IP : in T_Adresse_IP;
                              Masque : in T_Adresse_IP;
                              Port :  in out Unbounded_String;
                              Avancement : in Integer) return Boolean is
     begin
          if Est_Vide (Arbre) then
               return False;
          elsif Arbre.all.Feuille and ( ( IP and Masque )  = Arbre.all.Destination ) then
               Port := Arbre.all.Port;
               return True;
          elsif Arbre.all.Feuille and not ( ( IP and Masque ) = Arbre.all.Destination ) then
               return False;
          elsif not(Arbre.all.Feuille) and ( ( IP and 2**(31-Avancement) ) = 0 ) then
               return Comparer_Arbre(Arbre.all.Gauche, IP, Masque, Port, Avancement + 1);
          elsif not(Arbre.all.Feuille) and ( ( IP and 2**(31-Avancement) ) = 1 ) then
               return Comparer_Arbre(Arbre.all.Droite, IP, Masque, Port, Avancement + 1);
          end if;
     end Comparer_Arbre;

     procedure Enregistrer (Arbre : in out T_Arbre; Destination : in T_Adresse_IP;
                            Masque : in T_Adresse_IP;
                            Port : in Unbounded_String;
                            Rang : in Integer;
                            Avancement : in Integer) is
          Nouv_Cellule : T_Arbre;

     begin
          if Est_Vide (Arbre) then
               Nouv_Cellule := new T_Noeud'(Destination, Masque, Port, True, Null, Null, Rang);
               Arbre := Nouv_Cellule;

          elsif Arbre.all.Feuille and ( ( Destination and Arbre.all.Masque ) = Arbre.all.Destination ) then
               Arbre.all.Rang := Rang;

          elsif Arbre.all.Feuille and not( ( Destination and Arbre.all.Masque ) = Arbre.all.Destination ) then
               if ( ( Destination and 2**(31-Avancement) )  = 0 ) then
                    Nouv_Cellule := new T_Noeud;
                    Nouv_Cellule.all.Feuille := False;
                    Nouv_Cellule.all.Gauche := Arbre;
                    Nouv_Cellule.all.Droite := Null;
                    Nouv_Cellule.all.Rang := Rang;
                    Arbre := Nouv_Cellule;
               else
                    Nouv_Cellule := new T_Noeud;
                    Nouv_Cellule.all.Feuille := False;
                    Nouv_Cellule.all.Gauche := Null;
                    Nouv_Cellule.all.Droite := Arbre;
                    Nouv_Cellule.all.Rang := Rang;
                    Arbre := Nouv_Cellule;
               end if;
               Enregistrer (Arbre, Destination, Masque, Port, Rang, Avancement);

          elsif not(Arbre.all.Feuille) and ( ( Destination and 2**(31-Avancement) ) = 0 ) then
               Enregistrer (Arbre.all.Gauche, Destination, Masque, Port, Rang, Avancement + 1);

          elsif not(Arbre.all.Feuille) and ( ( Destination and 2**(31-Avancement) ) = 1 ) then
               Enregistrer (Arbre.all.Droite, Destination, Masque, Port, Rang, Avancement + 1);

          else
               raise Enregistrer_Exception;
          end if ;
     end Enregistrer;

     procedure Least_ranked (Arbre : in T_Arbre;
                            Destination : in out T_Adresse_IP;
                            Masque : in out T_Adresse_IP;
                            Min : in out Integer) is
          -- On initialiser Min à Chrono avant l'utilisation de Least_ranked
     begin
          if Est_Vide (Arbre) then
               null;
          elsif Arbre.all.Rang < Min then
               Destination := Arbre.all.Destination;
               Masque := Arbre.all.Port;
               Min := Arbre.all.Rang
          end if;
          Least_ranked(Arbre.all.Gauche, Destination, Masque, Min);
          Least_ranked(Arbre.all.Droite, Destination, Masque, Min);
     end Least_ranked;

     procedure Supprimer (Arbre : in T_Arbre; Desination : in T_Adresse_IP; Masque : in T_Adresse_IP) is

     begin
     end Supprimer;
     
     
end Prefix_Tree;
