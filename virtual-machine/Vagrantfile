# Use Vagrant to run a virtual machine for development and testing.
# retry the install, if there's a problem, with vagrant provision --provision-with resume
Vagrant.configure(2) do |config|
  # Use stock Ubuntu 14.04 Trusty, to match Travis Beta
  config.vm.box = "ubuntu/trusty64"
  config.vm.define :lswbox
  config.vm.network "public_network", bridge: "en0: Wi-Fi (AirPort)", mac: "020019620201"
  config.vm.synced_folder ".", "/vagrant"
  config.vm.provider "virtualbox" do |v|
    v.cpus = 4
    v.memory = 4096
   end
  # Run Ansible to provision this VM
  config.vm.provision :ansible do |ansible|
    ansible.playbook = "site.yml"
    ansible.host_key_checking = false
    ansible.extra_vars = { ansible_ssh_user: "vagrant", mode: "vagrant" }
  end
  # Picks up from any failed runs
  # Run this with: "vagrant provision --provision-with resume"
  config.vm.provision "resume", type: "ansible" do |resume|
    resume.playbook = "site.yml"
  end
end
