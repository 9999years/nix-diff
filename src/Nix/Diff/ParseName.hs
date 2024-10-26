module Nix.Diff.ParseName (parseDerivationPath) where

import Control.Monad (unless)
import Data.Char (isDigit)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import GHC.IsList (IsList (fromList))
import Text.Megaparsec (ErrorItem (..), ParseErrorBundle, Parsec, chunk, eof, failure, many, match, oneOf, runParser, satisfy, sepBy1, sepEndBy1, single, someTill, takeWhile1P, takeWhileP, try, (<|>), errorBundlePretty, takeP)
-- import Text.Regex.Posix

import Nix.Diff.Types

toText :: DerivationName -> Text
toText (RawName name) = name
toText NameAndVersion{..} = name <> "-" <> version

-- parseDerivationPath :: Text -> Either (ParseErrorBundle Text Void) DerivationName
parseDerivationPath :: Text -> DerivationName
parseDerivationPath input =
  case runParser parseNameAndVersion "Nix store path" input of
    Left err -> error $ errorBundlePretty err
    Right (name, versions) -> NameAndVersion {name, version = T.intercalate "." versions}

parseNameAndVersion :: Parser (Text, [Text])
parseNameAndVersion = do
        _ <- chunk "/nix/store/"

        hash <- takeP (Just "Nix store path hash") 32

        unless (T.all (`Set.member` nixBase32HashTokens) hash) do
          failure
            (Just $ Tokens $ fromList $ T.unpack hash)
            (Set.singleton $ Label $ fromList "A 32-character Nix base-32 hash")

        _ <- single '-'

        parseNameOrVersion

  where
    parseDrv = do 
      _ <- chunk ".drv"
      eof

    parseVersion :: Parser [Text]
    parseVersion = do
        component <- parseVersionComponent

        let dot = do
                single '.'

                components <- parseVersion

                pure (component : components)

        (parseDrv >> pure [component]) <|> dot

    parseNameOrVersion :: Parser (Text, [Text])
    parseNameOrVersion = do
        let version = do
                components <- parseVersion

                pure ("", components)

        let name = do
                component <- parseNameComponent

                let dot :: Parser (Text, [Text])
                    dot = do
                        single '.'

                        (rest, version') <- name

                        pure (component <> "." <> rest, version')

                    dash :: Parser (Text, [Text])
                    dash = do
                        single '-'
                        (rest, version') <- parseNameOrVersion

                        -- FIXME
                        let name' =
                                if T.null rest then component else component <> "-" <> rest

                        pure (name', version')

                (parseDrv >> pure ("", [component])) <|> dot <|> dash

        version <|> name

parseVersionComponent :: Parser Text
parseVersionComponent = do
    first <- takeWhile1P (Just "a digit") isDigit
    rest <-
      takeWhileP
        (Just "a version number")
        (\char -> char /= '-' && char /= '.')
    pure (first <> rest)

parseNameComponent :: Parser Text
parseNameComponent = takeWhile1P (Just "a derivation name") (\c -> c /= '-' && c /= '.')

-- matchDerivationPath :: String -> Maybe (String, String)
-- matchDerivationPath path =
  -- "^/nix/store/[0123456789abcdfghijklmnpqrsvwxyz]{32}-([^0-9][^-]*-)*([0-9][^.]*([.-][0-9][^.-]*))*\\.drv$" =~~ path


type Parser = Parsec Void Text

-- | Run a parser, discarding its result and returning the text it consumed while parsing.
recognize :: Parser a -> Parser Text
recognize parser = do
  (recognized, _result) <- match parser
  pure recognized

-- | See: https://github.com/kolloch/nix-base32/blob/d888813805f3c439eeb276a094972d77bb1282f8/src/lib.rs#L9
nixBase32HashTokens :: Set Char
nixBase32HashTokens = Set.fromList "0123456789abcdfghijklmnpqrsvwxyz"

-- storePathNameAndVersion :: Parser DerivationName
-- storePathNameAndVersion = do
  -- _ <- chunk "/nix/store/"

  -- hash <- takeP (Just "Nix store path hash") 32

  -- unless (T.all (`Set.member` nixBase32HashTokens) hash) do
    -- failure
      -- (Just $ Tokens $ fromList $ T.unpack hash)
      -- (Set.singleton $ Label $ fromList "A 32-character Nix base-32 hash")

  -- _ <- single '-'

  -- name <- T.init <$> recognize (sepEndBy1 nameFragment (single '-'))
  -- version <- T.init <$> recognize (sepEndBy1 versionFragment (oneOf ['.', '-']))
  -- _ <- drv

  -- pure NameAndVersion{name, version}

-- [> | Recognize a string of characters starting with a digit
-- and ending with a @-@.
-- -}
-- nameFragment :: Parser Text
-- nameFragment =
  -- recognize do
    -- _ <- satisfy (not . isDigit)
    -- takeWhile1P (Just "a derivation name") (/= '-')

-- -- | Recognize a digit and then a string of characters other than @-@ and @.@.
-- versionFragment :: Parser Text
-- versionFragment =
  -- recognize do
    -- _ <- takeWhile1P (Just "a digit") isDigit
    -- _ <-
      -- takeWhileP
        -- (Just "a version number")
        -- (\char -> char /= '-' && char /= '.')
    -- pure ()

-- -- | Recognize @drv@ at the end of input.
-- drv :: Parser Text
-- drv = do
  -- result <- chunk "drv"
  -- eof
  -- pure result
