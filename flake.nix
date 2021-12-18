{
  description = "elevator";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };
  };

  outputs = { self, nixpkgs }: {

    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      let src = nix-gitignore.gitignoreSource [] ./.;
      in haskell.packages.ghc901.callCabal2nix "elevator" src {};

    devShell.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      haskell.packages.ghc901.shellFor {
        buildInputs = with haskellPackages; [
          ghcid
          haskell-language-server
          hlint
          hnix
          implicit-hie
          rnix-lsp
        ];
        packages = haskellPackages: [
          self.defaultPackage.x86_64-linux
        ];
        withHoogle = true;
      };

  };
}
