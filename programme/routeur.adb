with Ada.Strings;               use Ada.Strings;	-- pour Both utilisé par Trim
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Exceptions;            use Ada.Exceptions;	-- pour Exception_Message
with LCA_IP; use LCA_IP;


procedure Routeur is

     --Définition des types secondaires
     type E_Politique is (FIFO,LRU,LFU);


     --Espace pour les sous-programmes

     -- Transformer un String en un Unbounded_String
     function "+" (Item : in String) return Unbounded_String
                   renames To_Unbounded_String;

     -- Transformer un Unbounded_String en un String
     function "-" (Item : in Unbounded_String) return String
                   renames To_String;

     -- Vérifier que le motif .txt est dans un Unbounded_String
     function txt_present (Mot : in Unbounded_String) return Boolean is
          Taille : Integer;
     begin
          Taille := Length(Mot);
          if Taille < 4 then
               return False;
          else
               return ( (Mot(Taille) = +"t") and (Mot(Taille-1) = +"x") and (Mot(Taille-2) = +"t") and (Mot(Taille-3) = +".") );
          end if;
     end txt_present;

     -- Variables globales du programme

     Paquet : File_Type;
     Nom_Paquet : Unbounded_String;
     Nom_Table : Unbounded_String;
     Nom_Resultat : Unbounded_String;
     Taille_Cache : Integer;
     Politique : E_Politique;
     Fin : Boolean;
     Bavard : Boolean;
     Nbr_Ajoute : Interger;


begin

     -- Initialisation des variables

     Nom_Paquet := "paquet.txt";
     Nom_Table := "table.txt";
     Nom_Resultat := "resultats.txt";
     Taille_Cache := 10;
     Politique := FIFO;
     Bavard := True;
     Fin := Faux;
     Nbr_Ajoute := 0;

     -- Traiter les arguments de la ligne de commande

     for a in 1..Argument_Count loop

          -- Traiter l'argument a de la ligne de commande

          case Argument(a) is
               when +"-c" => Taille_Cache := Argument(a+1);
               when +"-P" => Politique := Argument(a+1);
               when +"-S" => Bavard := False;
               when +"-p" => Nom_Paquet := Argument(a+1);
               when +"-t" => Nom_Table := Argument(a+1);
               when +"-r" => Nom_Resultat := Argument(a+1);
               when others => null;
          end case;

          -- Traiter les exceptions

          if Argument_Counte < 1 then

               raise Taille_Cache_Error;

          elsif txt_present(Nom_Paquet) then

               raise Not_Txt_Error;

          elsif txt_present(Nom_Table) then

               raise Not_Txt_Error;

          elsif txt_present(Nom_Cache) then

               raise Not_Txt_Error;
          end if;



end;
