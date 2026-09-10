{
  description = "Blog development environment";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-23-11.url = "github:NixOS/nixpkgs/nixos-23.11";
  };
  outputs = { self, nixpkgs, nixpkgs-23-11 }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
    in
    {
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };
          pkgs-23-11 = import nixpkgs-23-11 {
            inherit system;
          };
        in
        {
          default = pkgs.mkShell {
            packages = [
              pkgs.git
              pkgs.dig
              pkgs.whois
              pkgs.curl
              pkgs.jq
              pkgs.wget
              pkgs.gnumake
              pkgs.awscli2
              pkgs.zellij
              pkgs.neovim
              pkgs.htop
              pkgs.opencode
              # Fuckin' ruby. Bundle install only works with this version.
              pkgs-23-11.ruby_3_1
            ];
          };
        });
    };
}
