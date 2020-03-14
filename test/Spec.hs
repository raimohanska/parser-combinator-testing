import PackagesParser(readPackagesFromFile)
import Packages(Package, packageName, packageDescription, packageDependencies)
import Test.Hspec
import Data.Maybe(fromJust)
import Data.List(find)
import Data.Text(Text)

main :: IO ()
main = hspec $ do
    describe "Prelude.head" $ do
        it "returns the first element of a list" $ do
            head [23 ..] `shouldBe` (23 :: Int)
    describe "Reading packages" $ do
        it "Reads multiline description" $ do            
            pkg <- readPackage "libws-commons-util-java"
            packageDescription pkg `shouldBe` "Common utilities from the Apache Web Services Project\n\
                \This is a small collection of utility classes, that allow high\n\
                \performance XML processing based on SAX."
        it "Reads dependencies" $ do
            pkg <- readPackage "apparmor"
            packageDependencies pkg `shouldBe` [["libc6"], ["debconf", "debconf-2.0"], ["python"], ["lsb-base"], ["initramfs-tools"], ["debconf"]]

readPackage :: Text -> IO Package
readPackage name = do
    packages <- readPackagesFromFile "status.real.txt"
    return $ fromJust $ find ((== name) . packageName) packages
            
