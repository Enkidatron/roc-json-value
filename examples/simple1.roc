app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.12.0/1trwx8sltQ-e9Y2rOB4LWUWLS_sFVyETK8Twl0i9qpw.tar.gz",
    jv: "../package/main.roc",
}

import cli.Stdout
import jv.Value as JV

MsgId : [StringId Str, NumberId U64]

msg_id_recipe : JV.Recipe MsgId
msg_id_recipe = JV.one_of(
    [
        JV.map(JV.string, StringId),
        JV.map(JV.u64, NumberId),
    ],
)

msg_id_to_str : MsgId -> Str
msg_id_to_str = |msg_id|
    when msg_id is
        StringId str -> "The text '${str}'"
        NumberId num -> "The number ${Num.to_str(num)}"

main! = |_args|
    first_message =
        """
        {"method":"tools/call","params":{"name":"list_allowed_directories","arguments":{}},"jsonrpc":"2.0","id":16}
        """
    first_id = JV.decode_str(first_message, JV.field(msg_id_recipe, "id"))?
    second_message =
        """
        {"method":"tools/call","params":{"name":"list_allowed_directories","arguments":{}},"jsonrpc":"2.0","id":"17"}
        """
    second_id = JV.decode_str(second_message, JV.field(msg_id_recipe, "id"))?
    Stdout.line!("Decoded both ids: ${msg_id_to_str(first_id)} and ${msg_id_to_str(second_id)}")
