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
               -- On affiche de gauche à droite
               Afficher(Arbre.all.Gauche);

               -- Si on tombe sur une feuille, on affiche ses données
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
     function Rang_Max(Arbre : in T_Arbre) return Integer is

     begin
          if Est_Vide(Arbre) then
               return 0;
          elsif Arbre.all.Feuille and Arbre.all.Rang >  then

          end if;
     end Rang_Max;

     function Destination_Presente (Arbre : in T_Arbre ; Destination : in T_Adresse_IP) return Boolean is

     begin

     end Destination_Presente;

end Prefix_Tree;
