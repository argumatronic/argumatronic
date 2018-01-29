what nixops does is create the ec2 instance (or other vm or server) (using the aws key i have), install NixOS on that server, install nginx to run on that instance and make sure it runs on that server, and configure nginx. NixOS config and nginx config will both go into the same argumatronic.nix file. so that is your whole configuration! so now we can do all that without ssh'ing into the machine and using that goddamn nano to do our config editing.

but we still use hakyll to deploy the actual contents of the blog

so nixops builds all the config binaries on my machine and then just uploads them to the server so to update NixOS etc i probably need to update them on my machine and then re-deploy.

nixops create ./argumatronic.nix ./argumatronic-ec2.nix --deployment argumatronic

created deployment ‘18869063-c0e2-11e7-a6e5-507b9d44d4c4’
18869063-c0e2-11e7-a6e5-507b9d44d4c4

[julie@dolores:~/argumatronic/nixops]$ ls ~/.nixops
deployments.nixops --sqlite database!

[julie@dolores:~/argumatronic/nixops]$ nixops list -- lets you see list of the deployments you have 

nixops destroy then nixops delete i think deletes them from your database, should you mess up!

add ports to security group; 443 for https:

networking = {
  firewall = {
    allowedTCPPorts = [ 80 ];
  };
};

the one nixos config is an abstract config for any server; the ec2 specific one is for configuring that server specifically. so, like, there could be another config for digital ocean that could use the same abstract config combined with a droplet-specifc one to configure that instance.