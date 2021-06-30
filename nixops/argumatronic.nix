# nixos config for the server
{
  network.description = "Argumatronic";

  webserver =
    { pkgs, ... }:
    {
    	networking = {
  			firewall = {
    			allowedTCPPorts = [ 80 443 ];
  			};
		};
		# nix-deploy needs the deploying user to be able to sudo without a password
		security.sudo.wheelNeedsPassword = false;

    	services = {
		  nginx = {
		    enable = true;
		    user = "julie";
		    virtualHosts = {
		      "argumatronic.com" = {
		      	enableACME = true; # SSL should auto renew
		      	forceSSL = true;
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
          extraGroups = ["wheel"];
		      description = "Julie Moronuki";
		      uid = 1000;
		      openssh = {
		        authorizedKeys = {
		          keys = [
		          	#dolores
		            ''
		            ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDSnB6dODD8gWA0iaW0PPZSmQbPnevl8TNrq+xbziCeb1H4dizv2dcM9JVpokCPfnt5hn0bp01dpjDgDsMJxCpW4DfUd7/eM1q2EqaJ5wAqy5xhtxlv44xgFfJEjwr2GX/RKS8VOf6OSTGqtCEudpQA+vmQSEW9kdIII/KXjdwj+vn2fX/XwxuvTt9qCnh1TP1mdNjU5ZwSi4Yco++w+THnUFAZ/NP0iZZawLtKIIRRBRLEikRNWtcFDAnTVNE7O9eRlDotmm12qKqzQhDO1HANJ5E+QHOf+zh8Yg2aCwBuKd0Z19zusaCOUUjacuxKY8Lx3LOvpBi7QniicMhfqX/n jdog74@gmail.com
		            ''
		            #dorian
		            ''
		            ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDUA5QiRaMriKQrvA1H74UsJcnGieAcHYrHp27KBZjOhhKvinTkNyErY5JNFbgrCh3oC+6HeMSUOp+5qb2/hF0aO6kdYbGnYB3BchrbsPYFDccqW9eQONXdvYsa4mavfLzHIvXktkxVsd71CBRXqVFxxolYps99iJvFfhmgnn9Iz+zUIuOSRCPQ/Bcxbh3+fZHD2wlPpOZ352V3/utR6n6nlhK5W1Gq/mp5dmE32zCEOEse85xXQNxJpjdhIDZbu8PnNo2LuYIfRBFzIj/Gh6MIHJU3gQskt8MvKA+POdseflPLznxoDWYT3FqBO+agObr3FGnlnwpi1g9ym+U8T6SNFGUwR7cX5VkegLNSl7FTLMtaXoqUTKhBQH+ZIpNwlyepYnFo1BHR3IgsFDSaD/zLUjesBQIw6j+mtDCK9P3EnUcXo5v04OuGoyqTtAF4TTAz2kuC8ZsCs0cQEZRIoXqVIRcvPmlFr208o0SeGakCHVxIj6VnrQ+aHisVwuGAkq+Kb5mwE6b0xvuFGHo+bqHIUePymWQihbh0pZpYeSaJTLb2YG17HmE2rUFeO66CADmIMi2EWEI0xisT/e9FoqDcMnG/Z5sLBBDkerXwyfLA63zAkF0P3LtWX1Q0vYFhCn+VtMwE56qW/Z0sDdNiVwxn/a1GGh12gqXkNHBEePYjoQ== jdog74@gmail.com
		            ''

                #tristan
                ''
                ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCYvVsYGzkmlh0/C+7X5Uwe9NY3gUrd+5OfK72HJAqZJYAl/PCedgS/5c1Ncn+Yf0gr/UycFwitkZaP2OmPofcERg1JmHqCaLLsR48kZKZFYwGpCBWDH/cyoh3gYDhyJHj4I/bS75M0W48KTb8wiNRvfrMQ5T8XZ+PJprzz/cjNKkx1guUWGkwxFrfO8nb9YFK6Q5FDrk4O7C2hSKAAzfmMMj6F/htm4XVsr0g3ZBfq6TOWmpgKqKb6BMGhby10wDU2TWgWFyW61h1mld03ufZBHcOrGFEf3yx0IcmLW3Yd9a0obJEAIzrmAGi6+Kr/84xy3DNLBv6dwGqXZ62IErb5 julie@doriangray
                ''
		          ];
		        };
		      };
		    };
		  };
		};
    };
}
