{

  description = "A simple compiler from expressions to stack machine instruction";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
          ocamlPackages = pkgs.ocamlPackages;
      in
        {
          devShell = pkgs.mkShell {
            buildInputs = with ocamlPackages; [ pkgs.editorconfig-checker
                                                ocaml
                                                findlib
                                                dune_3
                                                menhir
                                                odoc
                                                utop
                                                core
                                              ];
          };
        }
    );
}
