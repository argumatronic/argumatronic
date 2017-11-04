let
  region = "us-east-1";
  accessKeyId = "julie"; # this is how it finds the aws key
in
  {
    webserver = { resources, ... }:
    {
      deployment = {
        targetEnv = "ec2";
        ec2 = {
          inherit accessKeyId region;
          instanceType = "t2.micro";
          keyPair = resources.ec2KeyPairs.my-key-pair;
          securityGroups = [ "web server" ];
          ebsInitialRootDiskSize = 30;
        };
      };
    };

    resources = {
      ec2KeyPairs = {
        my-key-pair = {
          inherit region accessKeyId;
        };
      };
    };
  }