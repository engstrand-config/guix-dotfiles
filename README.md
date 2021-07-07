# SSH key creation

Generate a key (press `Enter` on the prompts)
`ssh-keygen -t ed25519 -C "<you@email.com>"`
Start the SSH agent
`eval "$(ssh-agent -s)"`
Add your new key
`ssh-add ~/.ssh/id_ed25519`
Copy the public key to your clipboard
`xclip < ~/.ssh/id_ed25519.pub`

Paste the result (middle click) into e.g. Github.

[Per Mozilla's documentation](https://support.mozilla.org/en-US/kb/sync-custom-preferences), all custom control preferences must be created on every device.
Firefox Sync will not automatically copy custom control preferences to new devices.

To circumvent this problem, set `services.sync.prefs.dangerously_allow_arbitrary` on the new device and sync once (manually).
Now the custom control preferences will be synced to your new device.

In `user:config`:

Sync toolbar layout:
`services.sync.prefs.sync.browser.uiCustomization.state` to `true`
