package Routeur_Exceptions is

    Destination_Absente_Exception : Exception;	-- une clé est absente d'un Lca
    Taille_Cache_Exception : Exception;          -- la taille du chache est <1
    Not_Txt_Exception : Exception;               -- les nom de fichiers ne sont pas en .txt
    Not_Politique_Exception : Exception;         -- la politique n'est pas accepter

end Routeur_Exceptions;
