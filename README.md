
# aws-report

Silly program that replaces this shell script:

```bash
for region in $(aws ec2 describe-regions | jq --raw-output '.Regions[].RegionName'); do aws ec2 --region "${region}" describe-instances > instances-${region}; done
for instances in instances-*; do cat ${instances} | jq '.Reservations[].Instances[] | select(.State.Name == "running") | .KeyName'; done | sort | uniq -c
```

## Quick Start

* Download and install
  [Stack](http://docs.haskellstack.org/en/stable/README.html)
* `stack build`
* `stack exec aws-report`


