{
  outputs = { self, nixpkgs }: {
    packages.x86_64-linux = with nixpkgs.legacyPackages.x86_64-linux.pkgs; {
      bytestring = haskellPackages.callHackage "bytestring" "0.11.0.0" {};
      D1 = haskellPackages.callCabal2nix "D1" ./D1 {
        inherit (self.packages.x86_64-linux) utils;
      };
      D2 = haskellPackages.callCabal2nix "D2" ./D2 {
        inherit (self.packages.x86_64-linux) utils;
      };
      D3 = haskellPackages.callCabal2nix "D3" ./D3 {
        inherit (self.packages.x86_64-linux) utils;
      };
      D4 = haskellPackages.callCabal2nix "D4" ./D4 {
        inherit (self.packages.x86_64-linux) utils;
      };
      D5 = haskellPackages.callCabal2nix "D5" ./D5 {
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
