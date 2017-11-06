module Humbug.Output where

instance Show (Stmt a) where
  show (StPackage n) = "package " ++ n
 
  show (StImport n ns) = "import " ++ n ++ "." ++ showTypes ns
    where
      showTypes [] = "_"
      showTypes (n : []) = n
      showTypes ns = ("{" ++) . (++ "}") . concat . intersperse "," . foldr showType [] ns

      showType res (n, Nothing) = n : res
      showType res (n, Just a) = n ++ "=>" ++ a : res
  
      show (StPackageObject n stmts) = "package object " ++ n ++ (showStatements " " stmts)
  
  show (StSealedTrait n [] ccs) = "sealed trait " ++ n ++ "\n" ++ (unlines ccs)
  
  show (StCaseClass n as p) = "case class " ++ n ++ (showArgs as) ++ (showAncestor p)
  
  show (StCaseObject n as p) = "case object " ++ n ++ (showArgs as) ++ (showAncestor p)
  
  show (StCompanionObject n p stmts) = "object " ++ n ++ (showAncestor p) ++ " {\n" ++ (unlines stmts) ++ "\n}"

  show (StMethod n override as rt stmts) = showOverride ++ "def " ++ n ++ (showArgs as) ++ (showType rt) ++ (showStatements " = " stmts)
    where
      showOverride = if override then "override " else ""
      
  show (StCase v vt stmts) = "case " ++ v ++ (showType vt) ++ (showStatements " => " stmts)
  
  show (StTrait n stmts) = "trait " ++ n ++ (showStatements " " stmts)

  show (StArgument n at v) = n ++ (showType at) ++ (showValue v)

  show (StVal n override implicit stmts) = (showDecorators override implicit) ++ "val " ++ n ++  (showStatements " = " stmts)
    where
      showDecorators True False = "override "
      showDecorators False True = "implicit "
      showDecorators True True = "override implicit "
      showDecorators False False = ""

  show (StNew n caseclass vs stmts) = (showNew caseclass) ++ n ++ (showArgs vs) ++ (showStatements " " stmts)
    where
      showNew True = ""
      showNew False = "new "

  show (StLambda ps stmts) = ps ++ (showStatements " => " stmts)
      
  where
    showStatements _ [] = ""
    showStatements pfx (stmt : []) = pfx ++ stmt
    showStatements pfx stmts = pfx ++ "{\n" ++ (unlines stmts) ++ "\n}"
    
    showArgs [] = ""
    showArgs as = ("(" ++) . (++ ")") . intersperse "," . map show

    showType None = ""
    showType (Just rt) = ": " ++ rt

    showAncestor None = ""
    showAncestor (Just n) = " extends " ++ n

    showValue None = ""
    showValue (Just v) = " = " ++ v
