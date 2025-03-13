use linux_media as media;

fn main() {
    let info = media::MediaDeviceInfo::new("/dev/media0");
    match info {
        Ok(info) => println!("info: {:?}", info),
        Err(err) => println!("err: {}", err),
    }
    let topology = media::MediaTopology::new("/dev/media0");
    match topology {
        Ok(topology) => println!("topology: {:?}", topology),
        Err(err) => println!("err: {}", err),
    }
}
