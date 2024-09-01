exports.init = async function init(app)
{
    var existingHandle = null;
    app.ports.select_file_to_js.subscribe((a) => {
        let options = {
            types: [
              {
                description: "End-to-end test module",
                accept: {
                  "text/*": [".elm"],
                },
              },
            ],
            excludeAcceptAllOption: true,
            multiple: false,
          };
        window.showOpenFilePicker(options).then((result) => { existingHandle = result[0] });
    });

    app.ports.write_file_to_js.subscribe((text) =>
        {
            try {
                // create a new handle
                if (existingHandle) {
                    existingHandle.createWritable().then((writableStream) => {
                        writableStream.write("This is my file content").then((a) => {
                            writableStream.close();
                        });
                    });
                }
                else {
                    window.showSaveFilePicker().then((newHandle) => {
                        existingHandle = newHandle;
                        newHandle.createWritable().then((writableStream) => {
                                writableStream.write("This is my file content").then((a) => {
                                    writableStream.close();
                                });
                            });
                        });
                }
            } catch (err) {
                console.error(err.name, err.message);
            }
        }
    )

    //app.ports.copy_to_clipboard_to_js.subscribe(text => copyTextToClipboard(text));

    function copyTextToClipboard(text) {
      if (!navigator.clipboard) {
        fallbackCopyTextToClipboard(text);
        return;
      }
      navigator.clipboard.writeText(text).then(function() {
        // console.log('Async: Copying to clipboard was successful!');
      }, function(err) {
        console.error('Error: Could not copy text: ', err);
      });
    }

    function fallbackCopyTextToClipboard(text) {
      var textArea = document.createElement("textarea");
      textArea.value = text;

      // Avoid scrolling to bottom
      textArea.style.top = "0";
      textArea.style.left = "0";
      textArea.style.position = "fixed";

      document.body.appendChild(textArea);
      textArea.focus();
      textArea.select();

      try {
        var successful = document.execCommand('copy');
        if (successful !== true) {
          console.log('Error: Copying text command was unsuccessful');
        }
      } catch (err) {
        console.error('Error: Oops, unable to copy', err);
      }

      document.body.removeChild(textArea);
    }
}