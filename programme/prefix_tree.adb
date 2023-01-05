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

     -- Sous-programme utile pour d�terminer le rang maximum dans l'arbre
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

     function Destination_Presente (Arbre : in T_Arbre ; Destination : in T_Adresse_IP) return Boolean is
          Avancement : Integer;
          Copie_Arbre : T_Arbre;
     begin
          if Est_Vide (Arbre) then
               return False;
          else
               Copie_Arbre := Arbre;
               Avancement := 0;
               while not ( Est_Vide (Copie_Arbre.all.Droite) and Est_Vide (Copie_Arbre.all.Gauche) ) loop
                    if Copie_Arbre.all.Feuille and Copie_Arbre.all.Destination = Destination then
                         return True;
                    elsif ( Copie_Arbre.all.Destination and 2**(31-Avancement) ) = 0 then
                         Avancement := Avancement + 1;
                         Copie_Arbre := Copie_Arbre.all.Gauche;
                    else
                         Avancement := Avancement + 1;
                         Copie_Arbre := Copie_Arbre.all.Droite;
                    end if;
               end loop;
               return False;
          end if;
     end Destination_Presente;

     procedure Refresh (Arbre : in out T_Arbre; Destination : in T_Adresse_IP; Masque : in T_Adresse_IP) is
          Avancement : Integer;
          Copie_Arbre : T_Arbre;
          begin
          if not ( Destination_Presente (Arbre, Destination) ) then
               raise Destination_Absente_Exception;
          else
               Copie_Arbre := Arbre;
               Avancement := 0;
               while not ( Est_Vide (Copie_Arbre.all.Droite) and Est_Vide (Copie_Arbre.all.Gauche) ) loop
                    if Copie_Arbre.all.Feuille and Copie_Arbre.all.Destination = Destination then
                         Copie_ArbrE.all.Rang := Rang_Max(Arbre) + 1;
                    elsif ( Copie_Arbre.all.Destination and 2**(31-Avancement) ) = 0 then
                         Avancement := Avancement + 1;
                         Copie_Arbre := Copie_Arbre.all.Gauche;
                    else
                         Avancement := Avancement + 1;
                         Copie_Arbre := Copie_Arbre.all.Droite;
                    end if;
               end loop;
          end if;
     end Refresh;


end Prefix_Tree;
