{
  network.description = "Argumatronic";

  webserver =
    { pkgs, ... }:
    { 
    	networking = {
  			firewall = {
    			allowedTCPPorts = [ 80 ];
  			};
		};
    	services = {
		  nginx = {
		    enable = true;
		    user = "julie";
		    virtualHosts = {
		      "argumatronic.com" = {
		        locations = {
		          "/" = {
		            root = "/home/julie/argumatronic";
		          };
		        };
		      };
		    };
		  };
		};
		users = {
		  extraUsers = {
		    julie = {
		      isNormalUser = true;
		      description = "Julie Moronuki";
		      uid = 1000;
		      openssh = {
		        authorizedKeys = {
		          keys = [
		            ''
		            ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDSnB6dODD8gWA0iaW0PPZSmQbPnevl8TNrq+xbziCeb1H4dizv2dcM9JVpokCPfnt5hn0bp01dpjDgDsMJxCpW4DfUd7/eM1q2EqaJ5wAqy5xhtxlv44xgFfJEjwr2GX/RKS8VOf6OSTGqtCEudpQA+vmQSEW9kdIII/KXjdwj+vn2fX/XwxuvTt9qCnh1TP1mdNjU5ZwSi4Yco++w+THnUFAZ/NP0iZZawLtKIIRRBRLEikRNWtcFDAnTVNE7O9eRlDotmm12qKqzQhDO1HANJ5E+QHOf+zh8Yg2aCwBuKd0Z19zusaCOUUjacuxKY8Lx3LOvpBi7QniicMhfqX/n jdog74@gmail.com
		            ''
		          ];
		        };
		      };
		    };
		  };
		};
    };
}