defmodule SSV.Mixfile do
use Mix.Project

  def project do
    [
      app: :ssv,
      version: "0.1.0",
      description: "parse comma separated values. or tab separated values. or any kind of separated values",
      deps: deps(Mix.env),
      package: package,
      language: :erlang,
      erlc_options: opts(Mix.env)
    ]
  end

  defp opts(:dev), do: [d: :TEST] ++ opts(:prod)
  defp opts(_), do: []
  
  defp deps(_), do: [{:mixunit, "~> 0.9.2", only: :dev}]

  defp package do
    [
      files: [
        "LICENSE",
        "mix.exs",
        "src"
      ],
      contributors: ["alisdair sullivan"],
      links: %{"github" => "https://github.com/talentdeficit/ssv"},
      licenses: ["MIT"]
    ]
  end
end
