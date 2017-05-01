module Main where

import Text.XML.HXT.Core
import Text.XML.HXT.Curl

-- import Text.XML.HXT....   -- further HXT packages

import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit

main :: IO ()
main
    = do
      argv <- getArgs
      (al, src, dst) <- cmdlineOpts argv
      [rc]  <- runX (application al src dst)
      if rc >= c_err
	 then exitWith (ExitFailure (0-1))
	 else exitWith ExitSuccess

-- | the dummy for the boring stuff of option evaluation,
-- usually done with 'System.Console.GetOpt'

cmdlineOpts 	:: [String] -> IO (SysConfigList, String, String)
cmdlineOpts argv
    = return ([
    withValidate no
    ,withCurl []
    ], argv!!0, argv!!1)

-- | the main arrow

application	:: SysConfigList -> String -> String -> IOSArrow b Int
application cfg src dst
    = configSysVars cfg
      >>>
      readDocument [] src
      >>>
      processChildren (processDocumentRootElement `when` isElem)
      >>>
      writeDocument [withIndent yes, withOutputEncoding isoLatin1] dst
      >>>
      getErrStatus


-- | the dummy for the real processing: the identity filter

processDocumentRootElement	:: IOSArrow XmlTree XmlTree
processDocumentRootElement
    = addRefIcon         -- substitute this by the real application

selectAllText	:: ArrowXml a => a XmlTree XmlTree
selectAllText
    = deep isText

selectAllTextAndRealAltValues	:: ArrowXml a => a XmlTree XmlTree
selectAllTextAndRealAltValues
    = deep
      ( isText
	<+>
	( isElem >>> hasName "img"
	  >>>
	  getAttrValue "alt"
	  >>>
	  isA significant
	  >>>
	  arr addBrackets
	  >>>
	  mkText
	)
      )
    where
    significant :: String -> Bool
    significant = not . all (`elem` " \n\r\t")

    addBrackets :: String -> String
    addBrackets s
	=  " [[ " ++ s ++ " ]] "


addRefIcon	:: ArrowXml a => a XmlTree XmlTree
addRefIcon
    = processTopDown
      ( addImg
	`when`
	isExternalRef
      )
  where
  isExternalRef
	  = isElem
	    >>>
	    hasName "a"
      >>>
	    hasAttr "href"
	    >>>
	    getAttrValue "href"
	  where
	  isExtRef = True

  addImg
	  = replaceChildren
	    ( getChildren
	      <+>
	      imgElement
	    )

  imgElement
	  = mkelem "img"
	    [ sattr "src" "/icons/ref.png"
	    , sattr "alt" "external ref"
	    ] []