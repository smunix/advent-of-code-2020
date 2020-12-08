{
  outputs = { self, nixpkgs }: {
    packages.x86_64-linux = with nixpkgs.legacyPackages.x86_64-linux.pkgs; {
      D1 = haskellPackages.callCabal2nix "D1" ./D1 {
        inherit (self.packages.x86_64-linux) utils;
      };
      utils = haskellPackages.callCabal2nix "utils" ./utils {};
      d1b = haskellPackages.callPackage ./d1b/d1b.nix {
        version = ''${self.rev or self.lastModifiedDate}'';
      };
    };
    defaultPackage = builtins.mapAttrs (_: pkgs: pkgs.utils) self.packages;
  };
}
